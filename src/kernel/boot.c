/*
 * Copyright 2014, General Dynamics C4 Systems
 * Copyright 2021, HENSOLDT Cyber
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#include <assert.h>
#include <kernel/boot.h>
#include <kernel/thread.h>
#include <machine/io.h>
#include <machine/registerset.h>
#include <model/statedata.h>
#include <arch/machine.h>
#include <arch/kernel/boot.h>
#include <arch/kernel/vspace.h>
#include <linker.h>
#include <hardware.h>
#include <util.h>

/* (node-local) state accessed only during bootstrapping */
BOOT_BSS ndks_boot_t ndks_boot;

BOOT_BSS rootserver_mem_t rootserver;
BOOT_BSS static region_t rootserver_mem;
BOOT_BSS static p_region_t reg_freemem[MAX_NUM_FREEMEM_REG];
BOOT_BSS static p_region_t reg_reserved[MAX_NUM_FREEMEM_REG +
                                        NUM_RESERVED_REGIONS];

BOOT_BSS static word_t flag_print_active = 0;
BOOT_CODE static bool_t is_print_active(void) { return (0 != flag_print_active); }
BOOT_CODE void hack_enable_prints(void) { flag_print_active = 1; }

/*
 * Create a boot info record
 *
 * If bi is NULL, this just returns the amount of byte that would have been
 * written for the boot info record. If bi is not NULL and data is NULL, then
 * the boot info record payload is filled with zeros.
 */
BOOT_CODE seL4_Word add_extra_bootinfo(
    void *bi,
    seL4_Word bi_block_id,
    void const *data,
    seL4_Word data_len)
{
    if (bi) {
        /* The target location could be unaligned, thus we populate the header
         * on our aligned structure and then copy it to the target location.
         */
        seL4_BootInfoHeader header = {
            .id = bi_block_id,
            .len = sizeof(header) + data_len
        };

        memcpy(bi , &header, sizeof(header));

        void *addr = (void *)((word_t)bi + sizeof(header));
        if (data_len > 0)
        {
            if (data) {
                memcpy(addr, data, data_len);
            } else {
                memset(addr, 0, data_len);
            }
        }
    }

    return sizeof(seL4_BootInfoHeader) + data_len;
}

/*
 * Create a boot info record as padding for the given space. If there is not
 * enough space for a boot info header then just zero the space.
 *
 * If bi is NULL, this just returns the amount of byte that would have been
 * written for the boot info record.
 */
BOOT_CODE void add_extra_bootinfo_padding(
    void *bi,
    seL4_Word len)
{
    if (len < sizeof(seL4_BootInfoHeader)) {
        /* fill space with zeros if there is not even enough space to put a
         * header there
         */
        printf("WARNUNG: not enough left-over space to even write a boot info padding header");
        if (bi)
        {
            memset(bi, 0, len);
        }
    } else {
        /* ignore the return value */
        (void)add_extra_bootinfo(bi, SEL4_BOOTINFO_HEADER_PADDING, NULL,
                                 len - sizeof(seL4_BootInfoHeader));
    }
}

BOOT_CODE static word_t get_p_reg_size(p_region_t p_reg)
{
    word_t size = 0;
    if (p_reg.end >= p_reg.start) {
        size = p_reg.end - p_reg.start;
        if (0 != p_reg.end) {
            size++;
        }
    }
    return size;
}

BOOT_CODE static bool_t is_p_reg_empty(p_region_t p_reg)
{
    return (0 == get_p_reg_size(p_reg));
}

BOOT_CODE static bool_t is_reserved_slot_used(word_t idx)
{
    return (idx < ARRAY_SIZE(reg_reserved))
           && (!is_p_reg_empty(reg_reserved[idx]));
}

BOOT_CODE static pptr_t alloc_rootserver_obj(word_t size_bits, word_t n)
{
    pptr_t allocated = rootserver_mem.start;
    /* allocated memory must be aligned */
    assert(allocated % BIT(size_bits) == 0);
    rootserver_mem.start += (n * BIT(size_bits));
    /* we must not have run out of memory */
    assert(rootserver_mem.start <= rootserver_mem.end);
    memzero((void *) allocated, n * BIT(size_bits));
    return allocated;
}

BOOT_CODE static word_t rootserver_max_size_bits(word_t extra_bi_size_bits)
{
    word_t cnode_size_bits = CONFIG_ROOT_CNODE_SIZE_BITS + seL4_SlotBits;
    word_t max = MAX(cnode_size_bits, seL4_VSpaceBits);
    return MAX(max, extra_bi_size_bits);
}

BOOT_CODE static word_t calculate_rootserver_size(v_region_t it_v_reg, word_t extra_bi_size_bits)
{
    /* work out how much memory we need for root server objects */
    word_t size = BIT(CONFIG_ROOT_CNODE_SIZE_BITS + seL4_SlotBits);
    size += BIT(seL4_TCBBits); // root thread tcb
    size += BIT(seL4_PageBits); // ipc buf
    size += BIT(BI_FRAME_SIZE_BITS); // boot info
    size += BIT(seL4_ASIDPoolBits);
    size += extra_bi_size_bits > 0 ? BIT(extra_bi_size_bits) : 0;
    size += BIT(seL4_VSpaceBits); // root vspace
#ifdef CONFIG_KERNEL_MCS
    size += BIT(seL4_MinSchedContextBits); // root sched context
#endif
    /* for all archs, seL4_PageTable Bits is the size of all non top-level paging structures */
    return size + arch_get_n_paging(it_v_reg) * BIT(seL4_PageTableBits);
}

BOOT_CODE static void maybe_alloc_extra_bi(word_t cmp_size_bits, word_t extra_bi_size_bits)
{
    if (extra_bi_size_bits >= cmp_size_bits && rootserver.extra_bi == 0) {
        rootserver.extra_bi = alloc_rootserver_obj(extra_bi_size_bits, 1);
    }
}

/* Create pptrs for all root server objects in the memory region rootserver_mem
 * to cover the virtual memory region v_reg, and any extra boot info.
 */
BOOT_CODE static void create_rootserver_objects(v_region_t it_v_reg,
                                                word_t extra_bi_size_bits)
{
    /* Sanity checks: there must be a designated memory region. */
    assert(!is_reg_empty(rootserver_mem));

    /* the largest object the PD, the root cnode, or the extra boot info */
    word_t cnode_size_bits = CONFIG_ROOT_CNODE_SIZE_BITS + seL4_SlotBits;
    word_t max = rootserver_max_size_bits(extra_bi_size_bits);

    maybe_alloc_extra_bi(max, extra_bi_size_bits);

    /* the root cnode is at least 4k, so it could be larger or smaller than a pd. */
#if (CONFIG_ROOT_CNODE_SIZE_BITS + seL4_SlotBits) > seL4_VSpaceBits
    rootserver.cnode = alloc_rootserver_obj(cnode_size_bits, 1);
    maybe_alloc_extra_bi(seL4_VSpaceBits, extra_bi_size_bits);
    rootserver.vspace = alloc_rootserver_obj(seL4_VSpaceBits, 1);
#else
    rootserver.vspace = alloc_rootserver_obj(seL4_VSpaceBits, 1);
    maybe_alloc_extra_bi(cnode_size_bits, extra_bi_size_bits);
    rootserver.cnode = alloc_rootserver_obj(cnode_size_bits, 1);
#endif

    /* at this point we are up to creating 4k objects - which is the min size of
     * extra_bi so this is the last chance to allocate it */
    maybe_alloc_extra_bi(seL4_PageBits, extra_bi_size_bits);
    compile_assert(invalid_seL4_ASIDPoolBits, seL4_ASIDPoolBits == seL4_PageBits);
    rootserver.asid_pool = alloc_rootserver_obj(seL4_ASIDPoolBits, 1);
    rootserver.ipc_buf = alloc_rootserver_obj(seL4_PageBits, 1);
    compile_assert(invalid_BI_FRAME_SIZE_BITS, BI_FRAME_SIZE_BITS == seL4_PageBits);
    rootserver.boot_info = alloc_rootserver_obj(BI_FRAME_SIZE_BITS, 1);

    /* TCBs on aarch32 can be larger than page tables in certain configs */
#if seL4_TCBBits >= seL4_PageTableBits
    rootserver.tcb = alloc_rootserver_obj(seL4_TCBBits, 1);
#endif

    /* paging structures are 4k on every arch except aarch32 (1k) */
    word_t n = arch_get_n_paging(it_v_reg);
    rootserver.paging.start = alloc_rootserver_obj(seL4_PageTableBits, n);
    rootserver.paging.end = rootserver.paging.start + n * BIT(seL4_PageTableBits);

    /* for most archs, TCBs are smaller than page tables */
#if seL4_TCBBits < seL4_PageTableBits
    rootserver.tcb = alloc_rootserver_obj(seL4_TCBBits, 1);
#endif

#ifdef CONFIG_KERNEL_MCS
    rootserver.sc = alloc_rootserver_obj(seL4_MinSchedContextBits, 1);
#endif
    /* we should have allocated all our memory */
    assert(rootserver_mem.start == rootserver_mem.end);
}

BOOT_CODE void write_slot(slot_ptr_t slot_ptr, cap_t cap)
{
    slot_ptr->cap = cap;

    slot_ptr->cteMDBNode = nullMDBNode;
    mdb_node_ptr_set_mdbRevocable(&slot_ptr->cteMDBNode, true);
    mdb_node_ptr_set_mdbFirstBadged(&slot_ptr->cteMDBNode, true);
}

/* Our root CNode needs to be able to fit all the initial caps and not
 * cover all of memory.
 */
compile_assert(root_cnode_size_valid,
               CONFIG_ROOT_CNODE_SIZE_BITS < 32 - seL4_SlotBits &&
               BIT(CONFIG_ROOT_CNODE_SIZE_BITS) >= seL4_NumInitialCaps &&
               BIT(CONFIG_ROOT_CNODE_SIZE_BITS) >= (seL4_PageBits - seL4_SlotBits))

BOOT_CODE cap_t static create_root_cnode(void)
{
    cap_t cap = cap_cnode_cap_new(
                    CONFIG_ROOT_CNODE_SIZE_BITS, /* radix */
                    wordBits - CONFIG_ROOT_CNODE_SIZE_BITS, /* guard size */
                    0, /* guard */
                    rootserver.cnode); /* pptr */

    /* write the root CNode cap into the root CNode */
    write_slot(SLOT_PTR(rootserver.cnode, seL4_CapInitThreadCNode), cap);

    return cap;
}

/* Check domain scheduler assumptions. */
compile_assert(num_domains_valid,
               CONFIG_NUM_DOMAINS >= 1 && CONFIG_NUM_DOMAINS <= 256)
compile_assert(num_priorities_valid,
               CONFIG_NUM_PRIORITIES >= 1 && CONFIG_NUM_PRIORITIES <= 256)

BOOT_CODE static void create_domain_cap(cap_t root_cnode_cap)
{
    /* Check domain scheduler assumptions. */
    assert(ksDomScheduleLength > 0);
    for (word_t i = 0; i < ksDomScheduleLength; i++) {
        assert(ksDomSchedule[i].domain < CONFIG_NUM_DOMAINS);
        assert(ksDomSchedule[i].length > 0);
    }

    cap_t cap = cap_domain_cap_new();
    write_slot(SLOT_PTR(pptr_of_cap(root_cnode_cap), seL4_CapDomain), cap);
}

BOOT_CODE cap_t create_ipcbuf_frame_cap(cap_t root_cnode_cap, cap_t pd_cap, vptr_t vptr)
{
    clearMemory((void *)rootserver.ipc_buf, PAGE_BITS);

    /* create a cap of it and write it into the root CNode */
    cap_t cap = create_mapped_it_frame_cap(pd_cap, rootserver.ipc_buf, vptr, IT_ASID, false, false);
    write_slot(SLOT_PTR(pptr_of_cap(root_cnode_cap), seL4_CapInitThreadIPCBuffer), cap);

    return cap;
}

BOOT_CODE void create_bi_frame_cap(cap_t root_cnode_cap, cap_t pd_cap, vptr_t vptr)
{
    /* create a cap of it and write it into the root CNode */
    cap_t cap = create_mapped_it_frame_cap(pd_cap, rootserver.boot_info, vptr, IT_ASID, false, false);
    write_slot(SLOT_PTR(pptr_of_cap(root_cnode_cap), seL4_CapBootInfoFrame), cap);
}

BOOT_CODE word_t calculate_extra_bi_size_bits(word_t extra_size)
{
    if (extra_size == 0) {
        return 0;
    }

    word_t clzl_ret = clzl(ROUND_UP(extra_size, seL4_PageBits));
    word_t msb = seL4_WordBits - 1 - clzl_ret;
    /* If region is bigger than a page, make sure we overallocate rather than
     * underallocate
     */
    if (extra_size > BIT(msb)) {
        msb++;
    }
    return msb;
}

BOOT_CODE void populate_bi_frame(node_id_t node_id, word_t num_nodes,
                                 vptr_t ipcbuf_vptr, word_t extra_bi_size)
{
    /* clear boot info memory */
    clearMemory((void *)rootserver.boot_info, BI_FRAME_SIZE_BITS);
    if (extra_bi_size) {
        clearMemory((void *)rootserver.extra_bi,
                    calculate_extra_bi_size_bits(extra_bi_size));
    }

    /* initialise bootinfo-related global state */
    seL4_BootInfo *bi = BI_PTR(rootserver.boot_info);
    bi->nodeID = node_id;
    bi->numNodes = num_nodes;
    bi->numIOPTLevels = 0;
    bi->ipcBuffer = (seL4_IPCBuffer *)ipcbuf_vptr;
    bi->initThreadCNodeSizeBits = CONFIG_ROOT_CNODE_SIZE_BITS;
    bi->initThreadDomain = ksDomSchedule[ksDomScheduleIdx].domain;
    bi->extraLen = extra_bi_size;

    ndks_boot.bi_frame = bi;
    ndks_boot.slot_pos_cur = seL4_NumInitialCaps;
}

BOOT_CODE bool_t provide_cap(cap_t root_cnode_cap, cap_t cap)
{
    if (ndks_boot.slot_pos_cur >= BIT(CONFIG_ROOT_CNODE_SIZE_BITS)) {
        printf("ERROR: can't add another cap, all %"SEL4_PRIu_word
               " (=2^CONFIG_ROOT_CNODE_SIZE_BITS) slots used\n",
               BIT(CONFIG_ROOT_CNODE_SIZE_BITS));
        return false;
    }
    write_slot(SLOT_PTR(pptr_of_cap(root_cnode_cap), ndks_boot.slot_pos_cur), cap);
    ndks_boot.slot_pos_cur++;
    return true;
}

BOOT_CODE create_frames_of_region_ret_t create_frames_of_region(
    cap_t    root_cnode_cap,
    cap_t    pd_cap,
    region_t reg,
    bool_t   do_map,
    sword_t  pv_offset
)
{
    pptr_t     f;
    cap_t      frame_cap;
    seL4_SlotPos slot_pos_before;
    seL4_SlotPos slot_pos_after;

    slot_pos_before = ndks_boot.slot_pos_cur;

    for (f = reg.start; f < reg.end; f += BIT(PAGE_BITS)) {
        if (do_map) {
            frame_cap = create_mapped_it_frame_cap(pd_cap, f, pptr_to_paddr((void *)(f - pv_offset)), IT_ASID, false, true);
        } else {
            frame_cap = create_unmapped_it_frame_cap(f, false);
        }
        if (!provide_cap(root_cnode_cap, frame_cap)) {
            return (create_frames_of_region_ret_t) {
                .region  = S_REG_EMPTY,
                .success = false
            };
        }
    }

    slot_pos_after = ndks_boot.slot_pos_cur;

    return (create_frames_of_region_ret_t) {
        .region = (seL4_SlotRegion) {
            .start = slot_pos_before,
            .end   = slot_pos_after
        },
        .success = true
    };
}

BOOT_CODE cap_t create_it_asid_pool(cap_t root_cnode_cap)
{
    cap_t ap_cap = cap_asid_pool_cap_new(IT_ASID >> asidLowBits, rootserver.asid_pool);
    write_slot(SLOT_PTR(pptr_of_cap(root_cnode_cap), seL4_CapInitThreadASIDPool), ap_cap);

    /* create ASID control cap */
    write_slot(
        SLOT_PTR(pptr_of_cap(root_cnode_cap), seL4_CapASIDControl),
        cap_asid_control_cap_new()
    );

    return ap_cap;
}

#ifdef CONFIG_KERNEL_MCS
BOOT_CODE static bool_t configure_sched_context(tcb_t *tcb, sched_context_t *sc_pptr, ticks_t timeslice, word_t core)
{
    tcb->tcbSchedContext = sc_pptr;
    REFILL_NEW(tcb->tcbSchedContext, MIN_REFILLS, timeslice, 0, core);

    tcb->tcbSchedContext->scTcb = tcb;
    return true;
}

BOOT_CODE bool_t init_sched_control(cap_t root_cnode_cap, word_t num_nodes)
{
    seL4_SlotPos slot_pos_before = ndks_boot.slot_pos_cur;

    /* create a sched control cap for each core */
    for (unsigned int i = 0; i < num_nodes; i++) {
        if (!provide_cap(root_cnode_cap, cap_sched_control_cap_new(i))) {
            printf("can't init sched_control for node %u, provide_cap() failed\n", i);
            return false;
        }
    }

    /* update boot info with slot region for sched control caps */
    ndks_boot.bi_frame->schedcontrol = (seL4_SlotRegion) {
        .start = slot_pos_before,
        .end = ndks_boot.slot_pos_cur
    };

    return true;
}
#endif

BOOT_CODE bool_t create_idle_thread(void)
{
    pptr_t pptr;

#ifdef ENABLE_SMP_SUPPORT
    for (unsigned int i = 0; i < CONFIG_MAX_NUM_NODES; i++) {
#endif /* ENABLE_SMP_SUPPORT */
        pptr = (pptr_t) &ksIdleThreadTCB[SMP_TERNARY(i, 0)];
        NODE_STATE_ON_CORE(ksIdleThread, i) = TCB_PTR(pptr + TCB_OFFSET);
        configureIdleThread(NODE_STATE_ON_CORE(ksIdleThread, i));
#ifdef CONFIG_DEBUG_BUILD
        setThreadName(NODE_STATE_ON_CORE(ksIdleThread, i), "idle_thread");
#endif
        SMP_COND_STATEMENT(NODE_STATE_ON_CORE(ksIdleThread, i)->tcbAffinity = i);
#ifdef CONFIG_KERNEL_MCS
        bool_t result = configure_sched_context(NODE_STATE_ON_CORE(ksIdleThread, i), SC_PTR(&ksIdleThreadSC[SMP_TERNARY(i, 0)]),
                                                usToTicks(CONFIG_BOOT_THREAD_TIME_SLICE * US_IN_MS), SMP_TERNARY(i, 0));
        SMP_COND_STATEMENT(NODE_STATE_ON_CORE(ksIdleThread, i)->tcbSchedContext->scCore = i;)
        NODE_STATE_ON_CORE(ksIdleSC, i) = SC_PTR(&ksIdleThreadSC[SMP_TERNARY(i, 0)]);
        if (!result) {
            printf("Kernel init failed: Unable to allocate sc for idle thread\n");
            return false;
        }
#endif
#ifdef ENABLE_SMP_SUPPORT
    }
#endif /* ENABLE_SMP_SUPPORT */
    return true;
}

BOOT_CODE tcb_t *create_initial_thread(cap_t root_cnode_cap, cap_t it_pd_cap, vptr_t ui_v_entry, vptr_t bi_frame_vptr,
                                       vptr_t ipcbuf_vptr, cap_t ipcbuf_cap)
{
    tcb_t *tcb = TCB_PTR(rootserver.tcb + TCB_OFFSET);
#ifndef CONFIG_KERNEL_MCS
    tcb->tcbTimeSlice = CONFIG_TIME_SLICE;
#endif

    Arch_initContext(&tcb->tcbArch.tcbContext);

    /* derive a copy of the IPC buffer cap for inserting */
    deriveCap_ret_t dc_ret = deriveCap(SLOT_PTR(pptr_of_cap(root_cnode_cap), seL4_CapInitThreadIPCBuffer), ipcbuf_cap);
    if (dc_ret.status != EXCEPTION_NONE) {
        printf("Failed to derive copy of IPC Buffer\n");
        return NULL;
    }

    /* initialise TCB (corresponds directly to abstract specification) */
    cteInsert(
        root_cnode_cap,
        SLOT_PTR(pptr_of_cap(root_cnode_cap), seL4_CapInitThreadCNode),
        SLOT_PTR(rootserver.tcb, tcbCTable)
    );
    cteInsert(
        it_pd_cap,
        SLOT_PTR(pptr_of_cap(root_cnode_cap), seL4_CapInitThreadVSpace),
        SLOT_PTR(rootserver.tcb, tcbVTable)
    );
    cteInsert(
        dc_ret.cap,
        SLOT_PTR(pptr_of_cap(root_cnode_cap), seL4_CapInitThreadIPCBuffer),
        SLOT_PTR(rootserver.tcb, tcbBuffer)
    );
    tcb->tcbIPCBuffer = ipcbuf_vptr;

    setRegister(tcb, capRegister, bi_frame_vptr);
    setNextPC(tcb, ui_v_entry);

    /* initialise TCB */
#ifdef CONFIG_KERNEL_MCS
    if (!configure_sched_context(tcb, SC_PTR(rootserver.sc), usToTicks(CONFIG_BOOT_THREAD_TIME_SLICE * US_IN_MS), 0)) {
        return NULL;
    }
#endif

    tcb->tcbPriority = seL4_MaxPrio;
    tcb->tcbMCP = seL4_MaxPrio;
    tcb->tcbDomain = ksDomSchedule[ksDomScheduleIdx].domain;
#ifndef CONFIG_KERNEL_MCS
    setupReplyMaster(tcb);
#endif
    setThreadState(tcb, ThreadState_Running);

    ksCurDomain = ksDomSchedule[ksDomScheduleIdx].domain;
#ifdef CONFIG_KERNEL_MCS
    ksDomainTime = usToTicks(ksDomSchedule[ksDomScheduleIdx].length * US_IN_MS);
#else
    ksDomainTime = ksDomSchedule[ksDomScheduleIdx].length;
#endif
    assert(ksCurDomain < CONFIG_NUM_DOMAINS && ksDomainTime > 0);

#ifndef CONFIG_KERNEL_MCS
    SMP_COND_STATEMENT(tcb->tcbAffinity = 0);
#endif

    /* create initial thread's TCB cap */
    cap_t cap = cap_thread_cap_new(TCB_REF(tcb));
    write_slot(SLOT_PTR(pptr_of_cap(root_cnode_cap), seL4_CapInitThreadTCB), cap);

#ifdef CONFIG_KERNEL_MCS
    cap = cap_sched_context_cap_new(SC_REF(tcb->tcbSchedContext), seL4_MinSchedContextBits);
    write_slot(SLOT_PTR(pptr_of_cap(root_cnode_cap), seL4_CapInitThreadSC), cap);
#endif
#ifdef CONFIG_DEBUG_BUILD
    setThreadName(tcb, "rootserver");
#endif

    return tcb;
}

BOOT_CODE void init_core_state(tcb_t *scheduler_action)
{
#ifdef CONFIG_HAVE_FPU
    NODE_STATE(ksActiveFPUState) = NULL;
#endif
#ifdef CONFIG_DEBUG_BUILD
    /* add initial threads to the debug queue */
    NODE_STATE(ksDebugTCBs) = NULL;
    if (scheduler_action != SchedulerAction_ResumeCurrentThread &&
        scheduler_action != SchedulerAction_ChooseNewThread) {
        tcbDebugAppend(scheduler_action);
    }
    tcbDebugAppend(NODE_STATE(ksIdleThread));
#endif
    NODE_STATE(ksSchedulerAction) = scheduler_action;
    NODE_STATE(ksCurThread) = NODE_STATE(ksIdleThread);
#ifdef CONFIG_KERNEL_MCS
    NODE_STATE(ksCurSC) = NODE_STATE(ksCurThread->tcbSchedContext);
    NODE_STATE(ksConsumed) = 0;
    NODE_STATE(ksReprogram) = true;
    NODE_STATE(ksReleaseHead) = NULL;
    NODE_STATE(ksCurTime) = getCurrentTime();
#endif
}

BOOT_CODE static bool_t create_untyped_caps_for_phys_area(
    cap_t root_cnode_cap,
    seL4_SlotPos first_untyped_slot,
    bool_t is_device_memory,
    paddr_t start,
    word_t len
)
{
    if (len > 0) {
        printf("Create %s untypeds for [%"SEL4_PRIx_word"..%"SEL4_PRIx_word"]\n",
               is_device_memory ? "device" : "memory",
               start, start + len - 1);
    } else {
        printf("Create %s untypeds for zero-length region at  %"SEL4_PRIx_word"\n",
               is_device_memory ? "device" : "memory", start);
    }

    /* Due to integer limits, 'start + len' can overflow to 0 if the area
     * reaches to the end of the address space and this is also the end of the
     * integer range. This is handled in the the algorithm. Note that just
     * overflow to 0 is allowed, anything beyond that indicates something went
     * wrong with the configuration.
     */
    assert((start < (start + len)) || (0 == (start + len)));

    while (len > 0) {

        /* Determine maximum size 2^n block size within the region. Reduce to
         * region alignment if needed.
         */
        word_t align_bits = ctzl(start);
        word_t size_bits = seL4_WordBits - 1 - clzl(len);
        if (size_bits > align_bits) {
            size_bits = align_bits;
        }

        if (size_bits > seL4_MaxUntypedBits) {
            // printf("  cap 2^%d chunk to 2^%d\n",
            //        (int)size_bits, (int)seL4_MaxUntypedBits);
            size_bits = seL4_MaxUntypedBits;
        }
        /* The start address 0 satisfies any alignment needs, otherwise ensure
         * the region's bit size does not exceed the alignment of the region.
         */
        if (0 != reg.start) {
            unsigned int align_bits = ctzl(reg.start);
            if (size_bits > align_bits) {
                size_bits = align_bits;
            }
        }
        /* Provide an untyped capability for the region only if it is large
         * enough to be retyped into objects later. Otherwise the region can't
         * be used anyway.
         */
        if (size_bits >= seL4_MinUntypedBits) {

            word_t i = ndks_boot.slot_pos_cur - first_untyped_slot;
            if (i >= ARRAY_SIZE(ndks_boot.bi_frame->untypedList)) {
                printf("WARNING: ran out of available untyped cap slots\n");
                /* Do not consider this as a failure, some slots got populated
                 * and can be used by the system. */
                return true;
            }

            // printf("  list[%d] = 2^%d chunk [%p..%p]\n",
            //        (int)i, (int)size_bits, (void *)start,
            //        (void *)(start + BIT(size_bits) - 1));

            ndks_boot.bi_frame->untypedList[i] = (seL4_UntypedDesc) {
                .paddr    = start,
                .sizeBits = size_bits,
                .isDevice = is_device_memory,
                .padding  = {0}
            };

            /* Create a cap for the virtual address. */
            cap_t cap = cap_untyped_cap_new(MAX_FREE_INDEX(size_bits),
                                            is_device_memory,
                                            size_bits,
                                            WORD_REF(paddr_to_pptr(start)));
            if (!provide_cap(root_cnode_cap, cap)) {
                printf("ERROR: cloud not provide untyped cap for region\n");
                return false;
            }

            /* The cap way provided successfully and ndks_boot.slot_pos_cur was
             * updated.
             */
        } else {
            printf("WARNING: region too small: "
                   "[%"SEL4_PRIx_word" - %"SEL4_PRIx_word"]\n",
                   start, start + BIT(size_bits) - 1);
        }

        word_t chunk_len = BIT(size_bits);
        assert(chunk_len <= len); /* sanity check to catch coding errors */
        len -= chunk_len;
        /* An integer overflow for start can happen if we have reached the
         * end, this is fine if we will exit the loop anyway. */
        assert( ((start + chunk_len) > start) || (0 == len) );
        start += chunk_len;
    }

    return true;
}

BOOT_CODE bool_t create_untypeds(cap_t root_cnode_cap,
                                 paddr_t boot_mem_reuse_start,
                                 word_t boot_mem_reuse_len)
{
    printf("create_untypeds\n");

    seL4_SlotPos first_untyped_slot = ndks_boot.slot_pos_cur;

    paddr_t start = 0;
    for (word_t i = 0; is_reserved_slot_used(i); i++) {
        printf("ignore reserved region %d: [%"SEL4_PRIx_word"..%"SEL4_PRIx_word"]\n",
               (int)i, reg_reserved[i].start, reg_reserved[i].end - 1);
        p_region_t *reg = &reg_reserved[i];
        if (start < reg->start) {
            if (!create_untyped_caps_for_phys_area(root_cnode_cap,
                                                   first_untyped_slot, true,
                                                   start, reg->start - start)) {
                return false;
            }
        }
        start = reg->end;
    }

    if (start < CONFIG_PADDR_USER_DEVICE_TOP) {
        printf("create untypes up to CONFIG_PADDR_USER_DEVICE_TOP\n");
        word_t len = CONFIG_PADDR_USER_DEVICE_TOP - start;
        /* CONFIG_PADDR_USER_DEVICE_TOP could be at the end of the address space
         * and this might also be at the end of the integer range. Then the
         * inclusive form xxxff..ff' is used and we have to adapt the length.
         */
        if (CONFIG_PADDR_USER_DEVICE_TOP & 1) {
            len++;
        }
        if (!create_untyped_caps_for_phys_area(root_cnode_cap,
                                               first_untyped_slot, true, start,
                                               len)) {
            return false;
        }
    }

    /* One of the reserved regions is supposed to contain the kernel image.
     * Assuming that it is located in RAM, recycling the boot code and data area
     * into untyped memory caps is possible. It is up to the architecture
     * specific boot code to pass an area here for recycling or not, if
     * 'boot_mem_reuse_len' is 0, this will pass an do nothing.
     */
    if (!create_untyped_caps_for_phys_area(root_cnode_cap,
                                           first_untyped_slot, false,
                                           boot_mem_reuse_start,
                                           boot_mem_reuse_len)) {
       printf("ERROR: could not recycle boot memory "
              "[%"SEL4_PRIx_word"..%"SEL4_PRIx_word"]\n",
               boot_mem_reuse_start,
               boot_mem_reuse_start + boot_mem_reuse_len);
        return false;
    }

    /* Convert the free memory regions into untyped memory caps. */
    for (word_t i = 0; i < ARRAY_SIZE(reg_freemem); i++) {
        p_region_t *reg = &reg_freemem[i];
        /* Sanity check: region must be sane, it can be empty. */
        assert(reg->start <= reg->end);
        word_t len = reg->end - reg->start;
        if (len > 0) {
            /* fix length if end addresses uses inclusive form 'xxxff..ff' */
            if (reg->end & 1) {
                printf("WARNING: fix length\n");
                len++;
            }
            if (!create_untyped_caps_for_phys_area(root_cnode_cap,
                                                   first_untyped_slot, false,
                                                   reg->start, len)) {
                printf("ERROR: could not create untyped caps for free memory"
                       " at [%"SEL4_PRIx_word"..%"SEL4_PRIx_word"]\n",
                       reg->start, reg->start - 1 + len);
                return false;
            }
        }
        /* Mark slot as empty. */
        *reg = P_REG_EMPTY;
    } // end loop over reg_freemem

    ndks_boot.bi_frame->untyped = (seL4_SlotRegion) {
        .start = first_untyped_slot,
        .end   = ndks_boot.slot_pos_cur
    };

    return true;
}

BOOT_CODE void bi_finalise(void)
{
    ndks_boot.bi_frame->empty = (seL4_SlotRegion) {
        .start = ndks_boot.slot_pos_cur,
        .end   = BIT(CONFIG_ROOT_CNODE_SIZE_BITS)
    };
}

BOOT_CODE bool_t reserve_region(p_region_t reg)
{
    if (is_print_active()) {
        printf("reserve region [%"SEL4_PRIx_word"..%"SEL4_PRIx_word"]\n",
               reg.start, reg.end - 1);
    }

    /* Saity check: region must be sane. */
    assert(reg.start <= reg.end);

    /* if it is empty there is nothing to do. */
    if (is_p_reg_empty(reg)) {
        if (is_print_active()) {
            printf("  nothing to do for empty regions\n");
        }
        return true;
    }

    /* The list of regions is ordered properly and no regions are overlapping
     * or adjacent. Check if we have to
     * - insert the current region somewhere
     * - merge it with one or more existing regions
     * - append it at then end
     */
    word_t i;
    for (i = 0; is_reserved_slot_used(i); i++) {
        p_region_t *cur_reg = &reg_reserved[i];

        if (is_print_active()) {
            printf("  %d  [%"SEL4_PRIx_word"..%"SEL4_PRIx_word"]\n",
                   (int)i, cur_reg->start, cur_reg->end - 1);
        }

        if (reg.start > cur_reg->end) {
            /* Non-Overlapping case: ...|--cur_reg--|...|--reg--|...
             * The list or properly ordered, there is no impact on the current
             * region if new region is after it. Continue the loop with the next
             * reserved region.
             */
            if (is_print_active()) {
                printf("    skip\n");
            }
            continue;
        }

        if (reg.end < cur_reg->start) {
            /* Non-Overlapping case: ...|--reg--|...|--cur_reg--|...
             * The list or properly ordered, if the new element is before the
             * current element then we have to make space and insert it.
             */
            if (is_print_active()) {
                printf("    insert before\n");
            }
            word_t max = ARRAY_SIZE(reg_reserved);
            if (i == max - 1) {
                printf("ERROR: array is full with %d entries, can't insert "
                       "reserved [%"SEL4_PRIx_word"..%"SEL4_PRIx_word"]\n",
                       (int)ARRAY_SIZE(reg_reserved), reg.start, reg.end - 1);
                return false;
            }

            p_region_t saved_reg = reg;
            for (/*nothing */; /*nothing */; i++) {
                p_region_t tmp_reg = reg_reserved[i];
                reg_reserved[i] = saved_reg;
                if (is_p_reg_empty(tmp_reg)) {
                    break;
                }
                if (is_print_active()) {
                    printf("    move %d -> %d [%"SEL4_PRIx_word"..%"SEL4_PRIx_word"]\n",
                           (int)i, (int)(i + 1), tmp_reg.start, tmp_reg.end - 1);
                }
                saved_reg = tmp_reg;
            }

            return true;
        }

        /* For all remaining cases, the new region is adjacent or overlapping
         * with the current region, so we merge it.
         *
         * Case 1: |-reg-|             or |-reg-|           or |----reg-----|
         *         |<----|-cur_reg-|      |<--|-cur_reg-|      |<-|-cur_reg-|
         *
         * Case 2:           |-reg-|   or         |-reg-|   or |----reg-----|
         *         |-cur_reg-|---->|      |-cur_reg-|-->|      |-cur_reg-|->|
         *
         * Case 3a is a combination of case 1 and 2):  |--------reg--------|
         *                                             |<--|--cur_reg--|-->|
         *
         * Case 3b requires no work ):    |--reg--|
         *                              |--cur_reg--|
         */
        if (is_print_active()) {
            printf("    merge\n");
        }

        if (reg.start < cur_reg->start) {
            /* Case 1a-c: Adjust the region start. */
            cur_reg->start = reg.start;
        }

        if (reg.end > cur_reg->end) {
            /* Case 2a-c: Adjust the region end. */
            cur_reg->end = reg.end;
            /* We are not done, the new region could spawn more than just the
             * current region, so check how many of the following regions can
             * be merged.
             */
            i++;
            word_t j = i;
            for ( /*nothing */; is_reserved_slot_used(j); j++) {
                p_region_t *next_reg = &reg_reserved[j];
                if (cur_reg->end < next_reg->start) {
                    break;
                }
                if (is_print_active()) {
                    printf("    merge %d  [%"SEL4_PRIx_word"..%"SEL4_PRIx_word"]\n",
                           (int)j, next_reg->start, next_reg->end - 1);
                }
                /* new region reached into next region, merge them. */
                if (next_reg->end > reg.end)
                {
                    cur_reg->end = next_reg->end;
                }
            }

            if (j > i) {
                /* Move regions to close the gap. */
                for ( /*nothing */; is_reserved_slot_used(j); i++, j++) {
                    if (is_print_active()) {
                        printf("    move %d -> %d  "
                               "[%"SEL4_PRIx_word"..%"SEL4_PRIx_word"]\n",
                               (int)j, (int)i, cur_reg->start, cur_reg->end  - 1);
                    }
                    reg_reserved[i] = reg_reserved[j];
                }
                /* Mark remaining regions as empty. */
                for ( /*nothing */; is_reserved_slot_used(i); i++) {
                    if (is_print_active()) {
                        printf("    clear %d\n", (int)i);
                    }
                    reg_reserved[i] = P_REG_EMPTY;
                }
            }
        }

        return true;

    }

    /* If we arrive here, the new region is after the existing region. Append it
     * at the end if there is still space. */
    if (i >= ARRAY_SIZE(reg_reserved)) {
        printf("ERROR: can't reserve [%"SEL4_PRIx_word"..%"SEL4_PRIx_word"] "
               "as it execeeds MAX_NUM_RESV_REG (%d)\n",
               reg.start, reg.end - 1, (int)MAX_NUM_RESV_REG);
        return false;
    }
    if (is_print_active()) {
        printf("    put at %d\n", (int)i);
    }
    /* Sanity check: the slot must be empty. */
    assert(is_p_reg_empty(reg_reserved[i]));
    reg_reserved[i] = reg;
    return true;
}

/**
 * Dynamically initialise the available memory on the platform.
 * A region represents an area of memory.
 */
BOOT_CODE cap_t init_freemem(word_t n_available, const p_region_t *available,
                              v_region_t it_v_reg, word_t extra_bi_size_bits)
{
    hack_enable_prints();

    printf("Kernel memory layout\n");
    printf("  phys KERNEL_ELF_PADDR_BASE = %"SEL4_PRIx_word"\n", (word_t)KERNEL_ELF_PADDR_BASE);
    printf("  phys PADDR_USER_DEVICE_TOP = %"SEL4_PRIx_word"\n", (word_t)CONFIG_PADDR_USER_DEVICE_TOP);
    printf("  phys PADDR_TOP             = %"SEL4_PRIx_word"\n", PADDR_TOP);
    printf("       PPTR_BASE_OFFSET      = %"SEL4_PRIx_word"\n", (word_t)PPTR_BASE_OFFSET);
    printf("  virt USER_TOP              = %"SEL4_PRIx_word"\n", (word_t)USER_TOP);
    printf("  virt KERNEL_ELF_BASE       = %"SEL4_PRIx_word"\n", KERNEL_ELF_BASE);
    printf("  virt KDEV_BASE             = %"SEL4_PRIx_word"\n", KDEV_BASE);
    printf("  virt PPTR_TOP              = %"SEL4_PRIx_word"\n", PPTR_TOP);

    /* The system configuration is broken if no region is available */
    if (0 == n_available) {
        printf("ERROR: no memory regions available\n");
        return cap_null_cap_new();
    }

    /* Force ordering and exclusivity of available regions */
    printf("  available memory regions: %d\n", (int)n_available);
    for (word_t i = 0; i < n_available; i++) {
        const p_region_t *r = &available[i];

        printf("    [%"SEL4_PRIx_word"..%"SEL4_PRIx_word"]\n",
               r->start, r->end - 1);

        /* Available regions must be sane */
        if (r->start > r->end) {
            printf("ERROR: memory region %"SEL4_PRIu_word" has start > end\n", i);
            return cap_null_cap_new();
        }

        /* Available regions can't be empty. */
        if (r->start == r->end) {
            printf("ERROR: memory region %"SEL4_PRIu_word" empty\n", i);
            return cap_null_cap_new();
        }

        /* Regions must be ordered and must not overlap. */
        if ((i > 0) && (r->start < available[i - 1].end)) {
            printf("ERROR: memory region %d in wrong order\n", (int)i);
            return cap_null_cap_new();
        }
    }

    /* Populate the list of free memory regions. Cleaning the array is needed
     * semantically, but practically this does nothing. The bss was zeroed and
     * P_REG_EMPTY just contains zeros. Actually, strictly following the
     * semantics requires initializing reg_reserved with P_REG_EMPTY somewhere
     * also. Unfortunately, the platform boot code populates it before any
     * generic code runs, so here it's too late.
     */
    rootserver_mem = REG_EMPTY;
    for (word_t i = 0; i < ARRAY_SIZE(reg_freemem); i++) {
        reg_freemem[i] = P_REG_EMPTY;
    }

    word_t idx_f = 0;
    word_t idx_a = 0;
    word_t idx_r = 0;
    p_region_t a_chunk = P_REG_EMPTY;

    /* Iterate over the available regions and remove any reserved regions. At
     * end there might be a chunk from an available region that also needs to be
     * added. */
    while ((idx_a < n_available) || (!is_p_reg_empty(a_chunk))) {

        /* Either take the region from the list or the chunk of he region that
         * we have created in a previous iteration */
        const p_region_t *a = is_p_reg_empty(a_chunk) ? &available[idx_a]
                              : &a_chunk;
        /* Sanity check: region must be sane and not empty. */
        assert(a->start < a->end);
        /* This will be populated if we have found a new memory region that is
         * to be added to the free memory list. */
        p_region_t a_free = P_REG_EMPTY;
        /* We can simply add the whole available region (or the last chunk of
         * it if there are no more reserved region to check. Otherwise we have
         * to check the reserved region against the available region.
         */
        if (!is_reserved_slot_used(idx_r)) {
            a_free = *a;
            a_chunk = P_REG_EMPTY;
            idx_a++;
        } else {
            const p_region_t *r = &reg_reserved[idx_r];
            /* Sanity check: region must be sane and not empty. */
            assert(r->start < r->end);
            if (r->end <= a->start) {
                /* Skip a reserved region before the available region. */
                idx_r++;
            } else if (r->start >= a->end) {
                /* The reserved region starts after the available region (or the
                 * remaining chunk), we can use all of it
                 */
                a_free = *a;
                a_chunk = P_REG_EMPTY;
                idx_a++;
            } else {
                /* The reserved region overlaps with the available region (or
                 * the remaining chunk of it). If the reserved region starts
                 * within it, we can use the first part.
                 */
                if (r->start > a->start) {
                    a_free = (p_region_t){ .start = a->start, .end = r->start };
                };
                /* If the reserved region is fully within the available region,
                 * there is another chunk of available memory after wards. Check
                 * it in the next loop iteration against the next reserved
                 * region.
                 */
                if (r->end < a->end) {
                    a_chunk = (p_region_t){ .start = r->end, .end = a->end };
                    idx_r++;
                } else {
                    /* We have processed all of the available region, check the
                     * next available region in the next loo iteration.
                     */
                    a_chunk = P_REG_EMPTY;
                    idx_a++;
                }
            }
        }

        /* We may have found a memory region to add, otherwise just continue
         * checking regions. */
        if (is_p_reg_empty(a_free)) {
            continue;
        }
        /* Add the free memory region to the list of reserved regions. We will
         * later create device untyped caps for all non-reserved region.
         */
        if (!reserve_region(a_free)) {
            printf("ERROR: can't reserve free memory region "
                   "[%"SEL4_PRIx_word"..%"SEL4_PRIx_word"]\n",
                   a_free.start, a_free.end - 1);
            return cap_null_cap_new();
        }
        /* Reserving a region could modify the reserved regions list, so idx_r
         * is no longer valid. Start checking the reserved regions from the
         * start is the most simple solution. It's cheap since we skip a lot of
         * reserved regions, because they are before the current available
         * region (or the chunk) we are currently processing.
         */
        idx_r = 0;

        /* The kernel maps the physical memory completely in it's VA space,
         * which starts at PPTR_BASE. Unfortunately, VA space limitations could
         * prevent anything from the physical address space above PADDR_TOP to
         * be mapped, so there is no point in even adding it to the free memory
         * list. Usually, it's just an issue to worry about on 32-bit systems.
         */
        if (a_free.end > PADDR_TOP) {
            if (a_free.start >= PADDR_TOP) {
                printf("WARNING: can't use free memory region "
                       "[%"SEL4_PRIx_word"..%"SEL4_PRIx_word"], it is above "
                       "PADDR_TOP (%"SEL4_PRIx_word")\n",
                       a_free.start, a_free.end - 1, PADDR_TOP);
                /* Don't fail here, hopefully enough memory was available below
                 * PADDR_TOP. If not the boot process will fail later anyway.
                 */
                continue;
            }
            /* Limit the memory area at PADDR_TOP. */
            printf("WARNING: Capping memory region [%"SEL4_PRIx_word".."
                   "%"SEL4_PRIx_word"] at PADDR_TOP (%"SEL4_PRIx_word")\n",
                   a_free.start, a_free.end - 1, PADDR_TOP);
            printf("         %"SEL4_PRIu_word" (0x%"SEL4_PRIx_word") byte of "
                   "physical memory are not accessible via the kernel window\n",
                   a_free.end - PADDR_TOP, a_free.end - PADDR_TOP);
            a_free.end = PADDR_TOP;
        }

        /* Find a free slot. If none is available then don't fail, but assume
         * enough free memory regions are available already. The boot process
         * will fail later anyway if there is not enough memory.
         */
        if (idx_f >= ARRAY_SIZE(reg_freemem)) {
            printf("WARNING: can't add free memory region "
                   "[%"SEL4_PRIx_word"..%"SEL4_PRIx_word"], increase "
                   "MAX_NUM_FREEMEM_REG (currently %d)\n",
                   a_free.start, a_free.end - 1, (int)MAX_NUM_FREEMEM_REG);
            continue;
        }
        /* Convert the physical address region to a virtual address region and
         * add it at the free slot.
         */
        reg_freemem[idx_f] = a_free;
        idx_f++;

    } // loop while (idx_a < n_available)

    /* Sanity check: idx_f can't exceed the number of array elements. */
    assert(idx_f <= ARRAY_SIZE(reg_freemem));

    printf("free mem setup done\n");

    printf("  reserved regions\n");
    for (word_t i = 0; is_reserved_slot_used(i); i++) {
        p_region_t *r = &reg_reserved[i];
        printf("    [%"SEL4_PRIx_word"..%"SEL4_PRIx_word"]\n",
               r->start, r->end - 1);
        assert(r->start < r->end);
    }
    printf("  free mem:\n");
    for (word_t i = 0; i < ARRAY_SIZE(reg_freemem); i++) {
        p_region_t *r = &reg_freemem[i];
        if (r->start != r->end) {
            printf("    [%"SEL4_PRIx_word"..%"SEL4_PRIx_word"]\n",
                   r->start, r->end - 1);
        } else {
            printf("    [%"SEL4_PRIx_word"]\n", r->start);
        }
        assert(r->start <= r->end);
    }

    /* There must be at least one free memory region after carving out the
     * reserved regions. Without any memory we cannot even setup the root
     * server. */
    if (0 == idx_f) {
        printf("ERROR: no free memory\n");
        return cap_null_cap_new();
    }

    /* Now ndks_boot.freemem is set up to contain the usable memory. First thing
     * is creating the rootserver objects in it. Carve out the memory for these
     * objects. Ideally it can be carved out at the end of the last available
     * region, if this is not possible try the next lower regions. Split the
     * region in the space before the carved out memory and the space after it.
     */
    printf("Setting up rootserver objects...\n");
    word_t i = ARRAY_SIZE(reg_freemem) - 1;
    if (0 != reg_freemem[i].end) {
        printf("ERROR: Insufficient MAX_NUM_FREEMEM_REG (currently %d)\n",
               (int)MAX_NUM_FREEMEM_REG);
        return cap_null_cap_new();
    }

    /* Skip empty regions, we know there is at least one. */
    do {
        if (0 == i) {
            /* No memory at all? */
            return cap_null_cap_new();
        }
        i--;
    } while (is_p_reg_empty(reg_freemem[i]) && (i > 0));

    word_t size = calculate_rootserver_size(it_v_reg, extra_bi_size_bits);
    word_t max = rootserver_max_size_bits(extra_bi_size_bits);

    do {
        p_region_t *r = &reg_freemem[i];
        p_region_t *next = &reg_freemem[i + 1];
        /* Sanity check: the current region cannot be empty, the next one must
         * be empty. */
        assert(!is_p_reg_empty(*r));
        assert(is_p_reg_empty(*next));
        word_t reg_size = r->end - r->start;
        if (reg_size >= size) {
            paddr_t rootsrv_p_start = ROUND_DOWN(r->end - size, max);
            if (rootsrv_p_start >= r->start) {
                paddr_t rootsrv_p_end = rootsrv_p_start + size;
                /* Declare the region for the rootserver */
                rootserver_mem = paddr_to_pptr_reg((p_region_t) {
                    .start = rootsrv_p_start,
                    .end   = rootsrv_p_end
                });
                /* Declare a region of the unused memory afterwards - if there
                 * is any memory. */
                if (r->end > rootsrv_p_end) {
                    *next = (p_region_t) {
                        .start = rootsrv_p_end,
                        .end   = r->end
                    };
                }
                /* Cap the current region at the start of the rootserver memory
                 * region. */
                r->end = rootsrv_p_start;
                break;
            }
        }

        /* Sanity check: the next region slot must be empty. */
        assert(0 != next->end);
        *next = *r;
        *r = P_REG_EMPTY;

    } while (i-- > 0);

    printf("  rootserver_mem VA [%"SEL4_PRIx_word"..%"SEL4_PRIx_word"]\n",
           rootserver_mem.start, rootserver_mem.end - 1);

    if (is_reg_empty(rootserver_mem)) {
        printf("ERROR: Could not find a region big enough to fit all "
               "rootserver objects\n");
        return cap_null_cap_new();
    }

    /* Create the rootserver objects */
    create_rootserver_objects(it_v_reg, extra_bi_size_bits);

    /* Create the root c-node */
    cap_t root_cnode_cap = create_root_cnode();
    if (cap_get_capType(root_cnode_cap) == cap_null_cap) {
        printf("ERROR: root c-node creation failed\n");
        return cap_null_cap_new();
    }

    /* create the cap for managing thread domains */
    create_domain_cap(root_cnode_cap);

    /* initialise the IRQ states and provide the IRQ control cap */
    init_irqs(root_cnode_cap);

    return root_cnode_cap;
}
