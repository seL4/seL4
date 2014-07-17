/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(GD_GPL)
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
#include <arch/linker.h>
#include <plat/machine/hardware.h>

/* (node-local) state accessed only during bootstrapping */

ndks_boot_t ndks_boot BOOT_DATA;

BOOT_CODE bool_t
insert_region(region_t reg)
{
    unsigned int i;

    assert(reg.start <= reg.end);
    if (is_reg_empty(reg)) {
        return true;
    }
    for (i = 0; i < MAX_NUM_FREEMEM_REG; i++) {
        if (is_reg_empty(ndks_boot.freemem[i])) {
            ndks_boot.freemem[i] = reg;
            return true;
        }
    }
    return false;
}

BOOT_CODE static inline uint32_t
reg_size(region_t reg)
{
    return reg.end - reg.start;
}

BOOT_CODE pptr_t
alloc_region(uint32_t size_bits)
{
    unsigned int i;
    unsigned int reg_index = 0; /* gcc cannot work out that this will not be used uninitialized */
    region_t reg = REG_EMPTY;
    region_t rem_small = REG_EMPTY;
    region_t rem_large = REG_EMPTY;
    region_t new_reg;
    region_t new_rem_small;
    region_t new_rem_large;

    /* Search for a freemem region that will be the best fit for an allocation. We favour allocations
     * that are aligned to either end of the region. If an allocation must split a region we favour
     * an unbalanced split. In both cases we attempt to use the smallest region possible. In general
     * this means we aim to make the size of the smallest remaining region smaller (ideally zero)
     * followed by making the size of the largest remaining region smaller */

    for (i = 0; i < MAX_NUM_FREEMEM_REG; i++) {
        /* Determine whether placing the region at the start or the end will create a bigger left over region */
        if (ROUND_UP(ndks_boot.freemem[i].start, size_bits) - ndks_boot.freemem[i].start <
                ndks_boot.freemem[i].end - ROUND_DOWN(ndks_boot.freemem[i].end, size_bits)) {
            new_reg.start = ROUND_UP(ndks_boot.freemem[i].start, size_bits);
            new_reg.end = new_reg.start + BIT(size_bits);
        } else {
            new_reg.end = ROUND_DOWN(ndks_boot.freemem[i].end, size_bits);
            new_reg.start = new_reg.end - BIT(size_bits);
        }
        if (new_reg.end > new_reg.start &&
                new_reg.start >= ndks_boot.freemem[i].start &&
                new_reg.end <= ndks_boot.freemem[i].end) {
            if (new_reg.start - ndks_boot.freemem[i].start < ndks_boot.freemem[i].end - new_reg.end) {
                new_rem_small.start = ndks_boot.freemem[i].start;
                new_rem_small.end = new_reg.start;
                new_rem_large.start = new_reg.end;
                new_rem_large.end = ndks_boot.freemem[i].end;
            } else {
                new_rem_large.start = ndks_boot.freemem[i].start;
                new_rem_large.end = new_reg.start;
                new_rem_small.start = new_reg.end;
                new_rem_small.end = ndks_boot.freemem[i].end;
            }
            if ( is_reg_empty(reg) ||
                    (reg_size(new_rem_small) < reg_size(rem_small)) ||
                    (reg_size(new_rem_small) == reg_size(rem_small) && reg_size(new_rem_large) < reg_size(rem_large)) ) {
                reg = new_reg;
                rem_small = new_rem_small;
                rem_large = new_rem_large;
                reg_index = i;
            }
        }
    }
    if (is_reg_empty(reg)) {
        printf("Kernel init failing: not enough memory\n");
        return 0;
    }
    /* Remove the region in question */
    ndks_boot.freemem[reg_index] = REG_EMPTY;
    /* Add the remaining regions in largest to smallest order */
    insert_region(rem_large);
    if (!insert_region(rem_small)) {
        printf("alloc_region(): wasted 0x%x bytes due to alignment, try to increase MAX_NUM_FREEMEM_REG\n",
               (unsigned int)(rem_small.end - rem_small.start));
    }
    return reg.start;
}

BOOT_CODE void
write_slot(slot_ptr_t slot_ptr, cap_t cap)
{
    slot_ptr->cap = cap;

    slot_ptr->cteMDBNode = nullMDBNode;
    mdb_node_ptr_set_mdbRevocable  (&slot_ptr->cteMDBNode, true);
    mdb_node_ptr_set_mdbFirstBadged(&slot_ptr->cteMDBNode, true);
}

/* Our root CNode needs to be able to fit all the initial caps and not
 * cover all of memory.
 */
compile_assert(root_cnode_size_valid,
               CONFIG_ROOT_CNODE_SIZE_BITS < 32 - CTE_SIZE_BITS &&
               (1U << CONFIG_ROOT_CNODE_SIZE_BITS) >= BI_CAP_DYN_START)

BOOT_CODE cap_t
create_root_cnode(void)
{
    pptr_t  pptr;
    cap_t   cap;

    /* write the number of root CNode slots to global state */
    ndks_boot.slot_pos_max = BIT(CONFIG_ROOT_CNODE_SIZE_BITS);

    /* create an empty root CNode */
    pptr = alloc_region(CONFIG_ROOT_CNODE_SIZE_BITS + CTE_SIZE_BITS);
    if (!pptr) {
        printf("Kernel init failing: could not create root cnode\n");
        return cap_null_cap_new();
    }
    memzero(CTE_PTR(pptr), 1U << (CONFIG_ROOT_CNODE_SIZE_BITS + CTE_SIZE_BITS));
    cap =
        cap_cnode_cap_new(
            CONFIG_ROOT_CNODE_SIZE_BITS,      /* radix      */
            32 - CONFIG_ROOT_CNODE_SIZE_BITS, /* guard size */
            0,                                /* guard      */
            pptr                              /* pptr       */
        );

    /* write the root CNode cap into the root CNode */
    write_slot(SLOT_PTR(pptr, BI_CAP_IT_CNODE), cap);

    return cap;
}

compile_assert(irq_cnode_size, BIT(PAGE_BITS - CTE_SIZE_BITS) > maxIRQ)

BOOT_CODE bool_t
create_irq_cnode(void)
{
    pptr_t pptr;
    /* create an empty IRQ CNode */
    pptr = alloc_region(PAGE_BITS);
    if (!pptr) {
        printf("Kernel init failing: could not create irq cnode\n");
        return false;
    }
    memzero((void*)pptr, 1 << PAGE_BITS);
    intStateIRQNode = (cte_t*)pptr;
    return true;
}

/* Check domain scheduler assumptions. */
compile_assert(num_domains_valid,
               CONFIG_NUM_DOMAINS >= 1 && CONFIG_NUM_DOMAINS <= 256)
compile_assert(num_priorities_valid,
               CONFIG_NUM_PRIORITIES >= 1 && CONFIG_NUM_PRIORITIES <= 256)

BOOT_CODE void
create_domain_cap(cap_t root_cnode_cap)
{
    cap_t cap;
    unsigned int i;

    /* Check domain scheduler assumptions. */
    assert(ksDomScheduleLength > 0);
    for (i = 0; i < ksDomScheduleLength; i++) {
        assert(ksDomSchedule[i].domain < CONFIG_NUM_DOMAINS);
        assert(ksDomSchedule[i].length > 0);
    }

    cap = cap_domain_cap_new();
    write_slot(SLOT_PTR(pptr_of_cap(root_cnode_cap), BI_CAP_DOM), cap);
}


BOOT_CODE cap_t
create_ipcbuf_frame(cap_t root_cnode_cap, cap_t pd_cap, vptr_t vptr)
{
    cap_t cap;
    pptr_t pptr;

    /* allocate the IPC buffer frame */
    pptr = alloc_region(PAGE_BITS);
    if (!pptr) {
        printf("Kernel init failing: could not create ipc buffer frame\n");
        return cap_null_cap_new();
    }
    clearMemory((void*)pptr, PAGE_BITS);

    /* create a cap of it and write it into the root CNode */
    cap = create_mapped_it_frame_cap(pd_cap, pptr, vptr, IT_ASID, false);
    write_slot(SLOT_PTR(pptr_of_cap(root_cnode_cap), BI_CAP_IT_IPCBUF), cap);

    return cap;
}

BOOT_CODE void
create_bi_frame_cap(
    cap_t      root_cnode_cap,
    cap_t      pd_cap,
    pptr_t     pptr,
    vptr_t     vptr
)
{
    cap_t cap;

    /* create a cap of it and write it into the root CNode */
    cap = create_mapped_it_frame_cap(pd_cap, pptr, vptr, IT_ASID, false);
    write_slot(SLOT_PTR(pptr_of_cap(root_cnode_cap), BI_CAP_BI_FRAME), cap);
}

BOOT_CODE pptr_t
allocate_bi_frame(
    node_id_t  node_id,
    uint32_t   num_nodes,
    vptr_t ipcbuf_vptr
)
{
    pptr_t pptr;

    /* create the bootinfo frame object */
    pptr = alloc_region(BI_FRAME_SIZE_BITS);
    if (!pptr) {
        printf("Kernel init failed: could not allocate bootinfo frame\n");
        return 0;
    }
    clearMemory((void*)pptr, PAGE_BITS);

    /* initialise bootinfo-related global state */
    ndks_boot.bi_frame = BI_PTR(pptr);
    ndks_boot.slot_pos_cur = BI_CAP_DYN_START;

    BI_PTR(pptr)->node_id = node_id;
    BI_PTR(pptr)->num_nodes = num_nodes;
    BI_PTR(pptr)->num_iopt_levels = 0;
    BI_PTR(pptr)->ipcbuf_vptr = ipcbuf_vptr;
    BI_PTR(pptr)->it_cnode_size_bits = CONFIG_ROOT_CNODE_SIZE_BITS;
    BI_PTR(pptr)->it_domain = (uint8_t)ksDomSchedule[ksDomScheduleIdx].domain;

    return pptr;
}

BOOT_CODE bool_t
provide_cap(cap_t root_cnode_cap, cap_t cap)
{
    if (ndks_boot.slot_pos_cur >= ndks_boot.slot_pos_max) {
        printf("Kernel init failed: ran out of cap slots\n");
        return false;
    }
    write_slot(SLOT_PTR(pptr_of_cap(root_cnode_cap), ndks_boot.slot_pos_cur), cap);
    ndks_boot.slot_pos_cur++;
    return true;
}

BOOT_CODE create_frames_of_region_ret_t
create_frames_of_region(
    cap_t    root_cnode_cap,
    cap_t    pd_cap,
    region_t reg,
    bool_t   do_map,
    int32_t  pv_offset
)
{
    pptr_t     f;
    cap_t      frame_cap;
    slot_pos_t slot_pos_before;
    slot_pos_t slot_pos_after;

    slot_pos_before = ndks_boot.slot_pos_cur;

    for (f = reg.start; f < reg.end; f += BIT(PAGE_BITS)) {
        if (do_map) {
            frame_cap = create_mapped_it_frame_cap(pd_cap, f, f - BASE_OFFSET - pv_offset, IT_ASID, false);
        } else {
            frame_cap = create_unmapped_it_frame_cap(f, false);
        }
        if (!provide_cap(root_cnode_cap, frame_cap))
            return (create_frames_of_region_ret_t) {
            S_REG_EMPTY, false
        };
    }

    slot_pos_after = ndks_boot.slot_pos_cur;

    return (create_frames_of_region_ret_t) {
        (slot_region_t) { slot_pos_before, slot_pos_after }, true
    };
}

BOOT_CODE cap_t
create_it_asid_pool(cap_t root_cnode_cap)
{
    pptr_t ap_pptr;
    cap_t  ap_cap;

    /* create ASID pool */
    ap_pptr = alloc_region(ASID_POOL_SIZE_BITS);
    if (!ap_pptr) {
        printf("Kernel init failed: failed to create initial thread asid pool\n");
        return cap_null_cap_new();
    }
    memzero(ASID_POOL_PTR(ap_pptr), 1 << ASID_POOL_SIZE_BITS);
    ap_cap = cap_asid_pool_cap_new(IT_ASID >> asidLowBits, ap_pptr);
    write_slot(SLOT_PTR(pptr_of_cap(root_cnode_cap), BI_CAP_IT_ASID_POOL), ap_cap);

    /* create ASID control cap */
    write_slot(
        SLOT_PTR(pptr_of_cap(root_cnode_cap), BI_CAP_ASID_CTRL),
        cap_asid_control_cap_new()
    );

    return ap_cap;
}

BOOT_CODE bool_t
create_idle_thread(void)
{
    pptr_t pptr;
    pptr = alloc_region(TCB_BLOCK_SIZE_BITS);
    if (!pptr) {
        printf("Kernel init failed: Unable to allocate tcb for idle thread\n");
        return false;
    }
    memzero((void *)pptr, 1 << TCB_BLOCK_SIZE_BITS);
    ksIdleThread = TCB_PTR(pptr + TCB_OFFSET);
    configureIdleThread(ksIdleThread);
    return true;
}

BOOT_CODE bool_t
create_initial_thread(
    cap_t  root_cnode_cap,
    cap_t  it_pd_cap,
    vptr_t ui_v_entry,
    vptr_t bi_frame_vptr,
    vptr_t ipcbuf_vptr,
    cap_t  ipcbuf_cap
)
{
    pptr_t pptr;
    cap_t  cap;
    tcb_t* tcb;

    /* allocate TCB */
    pptr = alloc_region(TCB_BLOCK_SIZE_BITS);
    if (!pptr) {
        printf("Kernel init failed: Unable to allocate tcb for initial thread\n");
        return false;
    }
    memzero((void*)pptr, 1 << TCB_BLOCK_SIZE_BITS);
    tcb = TCB_PTR(pptr + TCB_OFFSET);
    tcb->tcbTimeSlice = CONFIG_TIME_SLICE;
    Arch_initContext(&tcb->tcbContext);

    /* initialise TCB (corresponds directly to abstract specification) */
    cteInsert(
        root_cnode_cap,
        SLOT_PTR(pptr_of_cap(root_cnode_cap), BI_CAP_IT_CNODE),
        SLOT_PTR(pptr, tcbCTable)
    );
    cteInsert(
        it_pd_cap,
        SLOT_PTR(pptr_of_cap(root_cnode_cap), BI_CAP_IT_PD),
        SLOT_PTR(pptr, tcbVTable)
    );
    cteInsert(
        ipcbuf_cap,
        SLOT_PTR(pptr_of_cap(root_cnode_cap), BI_CAP_IT_IPCBUF),
        SLOT_PTR(pptr, tcbBuffer)
    );
    tcb->tcbIPCBuffer = ipcbuf_vptr;
    setRegister(tcb, capRegister, bi_frame_vptr);
    setNextPC(tcb, ui_v_entry);

    /* initialise TCB */
    tcb->tcbPriority = seL4_MaxPrio;
    setupReplyMaster(tcb);
    setThreadState(tcb, ThreadState_Running);
    ksSchedulerAction = SchedulerAction_ResumeCurrentThread;
    ksCurThread = ksIdleThread;
    ksCurDomain = ksDomSchedule[ksDomScheduleIdx].domain;
    ksDomainTime = ksDomSchedule[ksDomScheduleIdx].length;
    assert(ksCurDomain < CONFIG_NUM_DOMAINS && ksDomainTime > 0);

    /* initialise current thread pointer */
    switchToThread(tcb); /* initialises ksCurThread */

    /* create initial thread's TCB cap */
    cap = cap_thread_cap_new(TCB_REF(tcb));
    write_slot(SLOT_PTR(pptr_of_cap(root_cnode_cap), BI_CAP_IT_TCB), cap);

    return true;
}

BOOT_CODE static bool_t
provide_untyped_cap(
    cap_t      root_cnode_cap,
    pptr_t     pptr,
    uint32_t   size_bits,
    slot_pos_t first_untyped_slot
)
{
    bool_t ret;
    unsigned int i = ndks_boot.slot_pos_cur - first_untyped_slot;
    if (i < CONFIG_MAX_NUM_BOOTINFO_UNTYPED_CAPS) {
        ndks_boot.bi_frame->ut_obj_paddr_list[i] = pptr_to_paddr((void*)pptr);
        ndks_boot.bi_frame->ut_obj_size_bits_list[i] = size_bits;
        ret = provide_cap(root_cnode_cap, cap_untyped_cap_new(0, size_bits, pptr));
    } else {
        printf("Kernel init: Too many untyped regions for boot info\n");
        ret = true;
    }
    return ret;
}

/**
  DONT_TRANSLATE
*/
BOOT_CODE static uint32_t boot_clz (uint32_t x)
{
    return CLZ (x);
}

/**
  DONT_TRANSLATE
*/
BOOT_CODE static uint32_t boot_ctz (uint32_t x)
{
    return CTZ (x);
}

BOOT_CODE static bool_t
create_untypeds_for_region(
    cap_t      root_cnode_cap,
    region_t   reg,
    slot_pos_t first_untyped_slot
)
{
    uint32_t align_bits;
    uint32_t size_bits;

    while (!is_reg_empty(reg)) {
        /* Determine the maximum size of the region */
        size_bits = WORD_BITS - 1 - boot_clz(reg.end - reg.start);

        /* Determine the alignment of the region */
        align_bits = boot_ctz(reg.start);

        /* Reduce size bits to align if needed */
        if (align_bits < size_bits) {
            size_bits = align_bits;
        }

        assert(size_bits >= WORD_BITS / 8);
        if (!provide_untyped_cap(root_cnode_cap, reg.start, size_bits, first_untyped_slot)) {
            return false;
        }
        reg.start += BIT(size_bits);
    }
    return true;
}

BOOT_CODE bool_t
create_untypeds(cap_t root_cnode_cap, region_t boot_mem_reuse_reg)
{
    slot_pos_t slot_pos_before;
    slot_pos_t slot_pos_after;
    uint32_t   i;
    region_t   reg;

    slot_pos_before = ndks_boot.slot_pos_cur;

    /* if boot_mem_reuse_reg is not empty, we can create UT objs from boot code/data frames */
    if (!create_untypeds_for_region(root_cnode_cap, boot_mem_reuse_reg, slot_pos_before)) {
        return false;
    }

    /* convert remaining freemem into UT objects and provide the caps */
    for (i = 0; i < MAX_NUM_FREEMEM_REG; i++) {
        reg = ndks_boot.freemem[i];
        ndks_boot.freemem[i] = REG_EMPTY;
        if (!create_untypeds_for_region(root_cnode_cap, reg, slot_pos_before)) {
            return false;
        }
    }

    slot_pos_after = ndks_boot.slot_pos_cur;
    ndks_boot.bi_frame->ut_obj_caps = (slot_region_t) {
        slot_pos_before, slot_pos_after
    };
    return true;
}

BOOT_CODE void
bi_finalise(void)
{
    slot_pos_t slot_pos_start = ndks_boot.slot_pos_cur;
    slot_pos_t slot_pos_end = ndks_boot.slot_pos_max;
    ndks_boot.bi_frame->null_caps = (slot_region_t) {
        slot_pos_start, slot_pos_end
    };
}
