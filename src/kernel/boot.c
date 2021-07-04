/*
 * Copyright 2014, General Dynamics C4 Systems
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

static bool_t print_active = 0;
BOOT_CODE void hack_enable_prints(void) { print_active = 1; }

BOOT_CODE static bool_t is_active_reserved(word_t idx)
{
    return (idx < ARRAY_SIZE(ndks_boot.reserved)) &&
           (0 != ndks_boot.reserved[idx].end);
}

BOOT_CODE bool_t reserve_region(p_region_t reg)
{
    if (print_active) {
        printf("reserve region [%"SEL4_PRIx_word"..%"SEL4_PRIx_word"]\n",
               reg.start, reg.end - 1);
    }
    if (reg.start > reg.end) {
        if (print_active) {
            printf("ERROR: invalid region\n");
        }
        return false;
    }

    /* Region must be sane, if it is empty there is nothing to do. */
    assert(reg.start <= reg.end);
    if (reg.start == reg.end) {
        if (print_active) {
            printf("  nothing to do for empty regions\n");
        }
        return true;
    }

    /* There is noting to do if the region has a zero size. */
    if (reg.start == reg.end) {
        return true;
    }

    /* The list of regions is ordered properly and no regions are overlapping
     * or adjacent. Check if we have to
     * - insert the current region somewhere
     * - merge it with one or more existing regions
     * - append it at then end
     */
    word_t i;
    for (i = 0; is_active_reserved(i); i++) {
        p_region_t *cur_reg = &ndks_boot.reserved[i];

        if (print_active) {
            printf("  %d  [%"SEL4_PRIx_word"..%"SEL4_PRIx_word"]\n",
                   (int)i, cur_reg->start, cur_reg->end - 1);
        }

        if (reg.start > cur_reg->end) {
            /* Non-Overlapping case: ...|--cur_reg--|...|--reg--|...
             * The list or properly ordered, there is no impact on the current
             * region if new region is after it. Continue the loop with the next
             * reserved region.
             */
            if (print_active) {
               printf("    skip\n");
            }
            continue;
        }

        if (reg.end < cur_reg->start) {
            /* Non-Overlapping case: ...|--reg--|...|--cur_reg--|...
             * The list or properly ordered, if the new element is before the
             * current element then we have to make space and insert it.
             */
            if (print_active) {
               printf("    insert before\n");
            }

            word_t max = ARRAY_SIZE(ndks_boot.reserved);
            if (i == max - 1) {
                printf("ERROR: can't insert reserved [%"SEL4_PRIx_word"..%"SEL4_PRIx_word"] "
                       "as the array is full with %d entries\n",
                       reg.start, reg.end - 1, (int)ARRAY_SIZE(ndks_boot.reserved));
                return false;
            }

            p_region_t saved_reg = reg;
            for (;;) {
                p_region_t tmp_reg = ndks_boot.reserved[i];
                ndks_boot.reserved[i] = saved_reg;
                if (0 == tmp_reg.end) {
                    break;
                }
                if (print_active) {
                    printf("    move %d -> %d [%"SEL4_PRIx_word"..%"SEL4_PRIx_word"]\n",
                           (int)i, (int)(i + 1), tmp_reg.start, tmp_reg.end - 1);
                }
                saved_reg = tmp_reg;
                i++;
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
        if (print_active) {
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
            for ( /*nothing */; is_active_reserved(j); j++) {
                p_region_t *next_reg = &ndks_boot.reserved[j];
                if (cur_reg->end < next_reg->start) {
                    break;
                }
                if (print_active) {
                    printf("    merge %d  [%"SEL4_PRIx_word"..%"SEL4_PRIx_word"]\n",
                           (int)j, next_reg->start, next_reg->end - 1);
                }
                /* new region reached into next region, merge them. */
                if (next_reg->end > reg.end)
                {
                    cur_reg->end = next_reg->end;
                }
            }

            if (j > i)
            {
                /* Move regions to close the gap. */
                for ( /*nothing */; is_active_reserved(j); i++, j++) {
                    if (print_active) {
                        cur_reg = &ndks_boot.reserved[j];
                        printf("    move %d -> %d  "
                               "[%"SEL4_PRIx_word"..%"SEL4_PRIx_word"]\n",
                               (int)j, (int)i, cur_reg->start,
                               cur_reg->end  - 1);
                    }
                    ndks_boot.reserved[i] = ndks_boot.reserved[j];
                }
                /* Mark remaining regions as empty. */
                for ( /*nothing */; is_active_reserved(i); i++) {
                    if (print_active) {
                        printf("    clear %d\n", (int)i);
                    }
                    ndks_boot.reserved[i] = P_REG_EMPTY;
                }
            }
        }

        return true;
    }

    /* If we arrive here, the new region is after the existing region. Append it
     * at the end. */
    if (i >= ARRAY_SIZE(ndks_boot.reserved)) {
        printf("ERROR: can't insert reserved [%"SEL4_PRIx_word"..%"SEL4_PRIx_word"] "
               "as the array is full with %d entries\n",
               reg.start, reg.end - 1, (int)ARRAY_SIZE(ndks_boot.reserved));
        return false;
    }

    /* insert or append region  */
    if (print_active) {
        printf("    append at %d\n", (int)i);
    }
    assert(0 == ndks_boot.reserved[i].end);
    ndks_boot.reserved[i] = reg;
    return true;
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
    size += 2 * BIT(seL4_PageBits); // boot info + ipc buf
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

/* Create pptrs for all root server objects, starting at a give start address,
 * to cover the virtual memory region v_reg, and any extra boot info.
 */
BOOT_CODE static bool_t create_rootserver_objects(v_region_t it_v_reg,
                                                  word_t extra_bi_size_bits)
{
    rootserver_mem = REG_EMPTY;

    /* Sanity check: there must be elements in the array - which is always the
     * case because we apply strict C rules and these do not allow empty arrays
     * anyway and compilation fails earlier already.
     */
    assert(0 != ARRAY_SIZE(ndks_boot.freemem));

    /* Carve out the memory for the root server objects. Ideally it can be
     * carved out at the end of the last available region, if this is not
     * possible try the next lower regions. Split the region in the space before
     * the carved out memory and the space after it.
     */
    word_t i = ARRAY_SIZE(ndks_boot.freemem) - 1;
    if (0 != ndks_boot.freemem[i].end) {
        printf("Insufficient MAX_NUM_FREEMEM_REG (currently %d)\n",
               (int)MAX_NUM_FREEMEM_REG);
        return false;
    }

    /* Skip empty regions. */
    do { /* nothing */ } while ((0 == ndks_boot.freemem[i].end) && (i-- > 0));

    word_t size = calculate_rootserver_size(it_v_reg, extra_bi_size_bits);
    word_t size_max_bits = rootserver_max_size_bits(extra_bi_size_bits);

    do {
        p_region_t *r = &ndks_boot.freemem[i];
        /* Sanity check: the region cannot be empty. */
        assert(0 != r->end);
        word_t reg_size = r->end - r->start;
        if (reg_size >= size) {
            paddr_t rootserver_start = ROUND_DOWN(r->end - size, size_max_bits);
            if (rootserver_start >= r->start) {
                if (i < ARRAY_SIZE(ndks_boot.freemem)) {
                    paddr_t rootserver_end = rootserver_start + size;
                    rootserver_mem = paddr_to_pptr_reg((p_region_t) {
                        .start = rootserver_start,
                        .end = rootserver_end
                    });
                    p_region_t *next = &ndks_boot.freemem[i + 1];
                    /* Sanity check: the next region slot must be empty. */
                    assert(0 == next->end);
                    if (r->end > rootserver_end)
                    {
                        *next = (p_region_t) {
                            .start = rootserver_end,
                            .end = r->end
                        };
                    }
                }
                /* Cap the current region at the start of the rootserver memory. */
                r->end = rootserver_start;
                break;
            }
        }

        if (i < ARRAY_SIZE(ndks_boot.freemem)) {
            p_region_t *next = &ndks_boot.freemem[i + 1];
            /* Sanity check: the next region slot must be empty. */
            assert(0 == next->end);
            *next = *r;
            *r = P_REG_EMPTY;
        }

    } while (i-- > 0);


//    bool_t can_retain = ((n_freemem + 1) < ARRAY_SIZE(ndks_boot.freemem));
//
//    do {
//        region_t *r = &ndks_boot.freemem[n_freemem];
//        word_t reg_size = r->end - r->start;
//        if (reg_size >= size) {
//            pptr_t start = ROUND_DOWN(r->end - size, size_max_bits);
//            if (start >= r->start) {
//                /* Sanity check: we can't exceed the existing region. */
//                assert(start + size <= r->end);
//                if (start == r->start) {
//                    /* rootserver object use the whole region. */
//                    rootserver_mem = *r;
//                } else if (can_retain) {
//                    /* create a new region for the rootserver objects */
//                    region_t *next = &ndks_boot.freemem[n_freemem + 1];
//                    /* Sanity check, the region must be empty. */
//                    assert(is_reg_empty(*next));
//                    rootserver_mem = (region_t) {
//                        .start = start,
//                        .end = r->end
//                    };
//                    *next = rootserver_mem;
//                    /* Shrink the current region. */
//                    r->end = start;
//                } else {
//                    printf("WARNING: Can't reuse left over memory of rootserver "
//                        "region, increase MAX_NUM_FREEMEM_REG (currently %d)\n",
//                        (int)MAX_NUM_FREEMEM_REG);
//                    /* We do not fail here, there might still be enough memory
//                     * available to run the system if there are more free memory
//                     * regions.
//                     */
//                    r->start = start;
//                    rootserver_mem = *r;
//                }
//                break;
//            }
//        }
//
//        /* The current region is too small, try the previous one. If we have
//         * enough slots available, Move an empty slot down to have space for the
//         * region split.
//         */
//        if (can_retain) {
//            region_t *next = &ndks_boot.freemem[n_freemem + 1];
//            /* Sanity check, the region must be empty. */
//            assert(is_reg_empty(*next));
//            *next = *r;
//            *r = REG_EMPTY;
//        }
//
//    } while (n_freemem-- > 0);

    if (is_reg_empty(rootserver_mem)) {
        printf("ERROR: Could not find a region big enough to fit all "
               "rootserver objects\n");
        return false;
    }

    /* the largest object the PD, the root cnode, or the extra boot info */
    word_t cnode_size_bits = CONFIG_ROOT_CNODE_SIZE_BITS + seL4_SlotBits;

    maybe_alloc_extra_bi(size_max_bits, extra_bi_size_bits);

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
    rootserver.asid_pool = alloc_rootserver_obj(seL4_ASIDPoolBits, 1);
    rootserver.ipc_buf = alloc_rootserver_obj(seL4_PageBits, 1);
    rootserver.boot_info = alloc_rootserver_obj(seL4_PageBits, 1);

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

    return true;
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
    /* write the number of root CNode slots to global state */
    ndks_boot.slot_pos_max = BIT(CONFIG_ROOT_CNODE_SIZE_BITS);

    cap_t cap =
        cap_cnode_cap_new(
            CONFIG_ROOT_CNODE_SIZE_BITS,      /* radix      */
            wordBits - CONFIG_ROOT_CNODE_SIZE_BITS, /* guard size */
            0,                                /* guard      */
            rootserver.cnode              /* pptr       */
        );

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
    /* If region is bigger than a page, make sure we overallocate rather than underallocate */
    if (extra_size > BIT(msb)) {
        msb++;
    }
    return msb;
}

BOOT_CODE void populate_bi_frame(node_id_t node_id, word_t num_nodes, vptr_t ipcbuf_vptr,
                                 word_t extra_bi_size)
{
    clearMemory((void *) rootserver.boot_info, BI_FRAME_SIZE_BITS);
    if (extra_bi_size) {
        clearMemory((void *) rootserver.extra_bi, calculate_extra_bi_size_bits(extra_bi_size));
    }

    /* initialise bootinfo-related global state */
    ndks_boot.bi_frame = BI_PTR(rootserver.boot_info);
    ndks_boot.slot_pos_cur = seL4_NumInitialCaps;
    BI_PTR(rootserver.boot_info)->nodeID = node_id;
    BI_PTR(rootserver.boot_info)->numNodes = num_nodes;
    BI_PTR(rootserver.boot_info)->numIOPTLevels = 0;
    BI_PTR(rootserver.boot_info)->ipcBuffer = (seL4_IPCBuffer *) ipcbuf_vptr;
    BI_PTR(rootserver.boot_info)->initThreadCNodeSizeBits = CONFIG_ROOT_CNODE_SIZE_BITS;
    BI_PTR(rootserver.boot_info)->initThreadDomain = ksDomSchedule[ksDomScheduleIdx].domain;
    BI_PTR(rootserver.boot_info)->extraLen = extra_bi_size;
}

BOOT_CODE bool_t provide_cap(cap_t root_cnode_cap, cap_t cap)
{
    if (ndks_boot.slot_pos_cur >= ndks_boot.slot_pos_max) {
        printf("Kernel init failed: ran out of cap slots\n");
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
        if (!provide_cap(root_cnode_cap, frame_cap))
            return (create_frames_of_region_ret_t) {
            S_REG_EMPTY, false
        };
    }

    slot_pos_after = ndks_boot.slot_pos_cur;

    return (create_frames_of_region_ret_t) {
        (seL4_SlotRegion) { slot_pos_before, slot_pos_after }, true
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

BOOT_CODE static bool_t provide_untyped_cap(
    cap_t      root_cnode_cap,
    bool_t     device_memory,
    pptr_t     pptr,
    word_t     size_bits,
    seL4_SlotPos first_untyped_slot
)
{
    bool_t ret;
    cap_t ut_cap;
    word_t i = ndks_boot.slot_pos_cur - first_untyped_slot;
    if (i < CONFIG_MAX_NUM_BOOTINFO_UNTYPED_CAPS) {
        ndks_boot.bi_frame->untypedList[i] = (seL4_UntypedDesc) {
            pptr_to_paddr((void *)pptr), size_bits, device_memory, {0}
        };
        ut_cap = cap_untyped_cap_new(MAX_FREE_INDEX(size_bits),
                                     device_memory, size_bits, pptr);
        ret = provide_cap(root_cnode_cap, ut_cap);
    } else {
        printf("Kernel init: Too many untyped regions for boot info\n");
        ret = true;
    }
    return ret;
}

BOOT_CODE static bool_t create_untypeds_for_region(
    cap_t      root_cnode_cap,
    bool_t     device_memory,
    region_t   reg,
    seL4_SlotPos first_untyped_slot
)
{
    word_t align_bits;
    word_t size_bits;

    while (!is_reg_empty(reg)) {
        /* Determine the maximum size of the region */
        size_bits = seL4_WordBits - 1 - clzl(reg.end - reg.start);

        /* Determine the alignment of the region */
        if (reg.start != 0) {
            align_bits = ctzl(reg.start);
        } else {
            align_bits = size_bits;
        }
        /* Reduce size bits to align if needed */
        if (align_bits < size_bits) {
            size_bits = align_bits;
        }
        if (size_bits > seL4_MaxUntypedBits) {
            size_bits = seL4_MaxUntypedBits;
        }

        if (size_bits >= seL4_MinUntypedBits) {
            if (!provide_untyped_cap(root_cnode_cap, device_memory, reg.start, size_bits, first_untyped_slot)) {
                return false;
            }
        }
        reg.start += BIT(size_bits);
    }
    return true;
}

BOOT_CODE bool_t create_untypeds(cap_t root_cnode_cap,
                                 region_t boot_mem_reuse_reg)
{
    seL4_SlotPos first_untyped_slot = ndks_boot.slot_pos_cur;

    paddr_t start = 0;
    for (word_t i = 0; is_active_reserved(i); i++) {
        p_region_t *cur_reg = &ndks_boot.reserved[i];
        if (start < cur_reg->start) {
            region_t reg = paddr_to_pptr_reg((p_region_t) {
                .start = start,
                .end   = cur_reg->start
            });
            if (!create_untypeds_for_region(root_cnode_cap, true, reg,
                                            first_untyped_slot)) {
                return false;
            }
        }

        start = cur_reg->end;
    }

    if (start < CONFIG_PADDR_USER_DEVICE_TOP) {
        region_t reg = paddr_to_pptr_reg((p_region_t) {
            .start = start,
            .end   = CONFIG_PADDR_USER_DEVICE_TOP
        });
        /*
         * The auto-generated bitfield code will get upset if the
         * end pptr is larger than the maximum pointer size for this architecture.
         */
        if (reg.end > PPTR_TOP) {
            reg.end = PPTR_TOP;
        }
        if (!create_untypeds_for_region(root_cnode_cap, true, reg, first_untyped_slot)) {
            return false;
        }
    }

    /* if boot_mem_reuse_reg is not empty, we can create UT objs from boot code/data frames */
    if (!create_untypeds_for_region(root_cnode_cap, false, boot_mem_reuse_reg, first_untyped_slot)) {
        return false;
    }

    /* convert remaining freemem into UT objects and provide the caps */
    for (word_t i = 0; i < ARRAY_SIZE(ndks_boot.freemem); i++) {
        region_t reg = paddr_to_pptr_reg(ndks_boot.freemem[i]);
        ndks_boot.freemem[i] = P_REG_EMPTY;
        if (!create_untypeds_for_region(root_cnode_cap, false, reg, first_untyped_slot)) {
            return false;
        }
    }

    ndks_boot.bi_frame->untyped = (seL4_SlotRegion) {
        .start = first_untyped_slot,
        .end   = ndks_boot.slot_pos_cur
    };

    return true;
}

BOOT_CODE void bi_finalise(void)
{
    seL4_SlotPos slot_pos_start = ndks_boot.slot_pos_cur;
    seL4_SlotPos slot_pos_end = ndks_boot.slot_pos_max;
    ndks_boot.bi_frame->empty = (seL4_SlotRegion) {
        slot_pos_start, slot_pos_end
    };
}

/**
 * Dynamically initialise the available memory on the platform.
 * A region represents an area of memory.
 */
BOOT_CODE cap_t init_freemem(word_t n_available, const p_region_t *available,
                              v_region_t it_v_reg, word_t extra_bi_size_bits)
{
    print_active = 1;

    printf("Kernel memory layout\n");
    printf("  phys KERNEL_ELF_PADDR_BASE = %"SEL4_PRIx_word"\n", (word_t)KERNEL_ELF_PADDR_BASE);
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

        /* Available regions can't be empty */
        if (r->start == r->end) {
            printf("ERROR: memory region %"SEL4_PRIu_word" empty\n", i);
            return cap_null_cap_new();
        }

        /* regions must be ordered and must not overlap */
        if ((i > 0) && (r->start < available[i - 1].end)) {
            printf("ERROR: memory region %d in wrong order\n", (int)i);
            return cap_null_cap_new();
        }
    }

    printf("  reserved regions\n");
    for (word_t i = 0; is_active_reserved(i); i++) {
        p_region_t *r = &ndks_boot.reserved[i];
        printf("    [%"SEL4_PRIx_word"..%"SEL4_PRIx_word"]\n",
               r->start, r->end - 1);
        assert(r->start < r->end);
    }

    /* Clear the free memory list. This is needed semantically, but practically
     * it does nothing. The bss was zeroed and P_REG_EMPTY just contains zeros.
     * Actually, we should initialize ndks_boot.reserved with P_REG_EMPTY also
     * somewhere to stick to the semantics. Unfortunately, the platform boot
     * code populates it before any generic code runs, so here it's too late.
     */
    for (word_t i = 0; i < ARRAY_SIZE(ndks_boot.freemem); i++) {
        ndks_boot.freemem[i] = P_REG_EMPTY;
    }

    printf("loop to process reserved and available regions\n");
    word_t idx_f = 0;
    word_t idx_a = 0;
    word_t idx_r = 0;
    p_region_t a_chunk = P_REG_EMPTY;

    /* Iterate through the available regions, removing any reserved regions. */
    while (idx_a < n_available) {

        /* Either take the region from the list or the chunk of he region that
         * we have created in a previous iteration */
        const p_region_t *a = (0 == a_chunk.end) ? &available[idx_a] : &a_chunk;

        /* There can't be any invalid or empty region in the list. */
        assert(a->start < a->end);

        /* This will be populated if we have found a new memory region that is
         * to be added to the free memory list. */
        p_region_t insert = P_REG_EMPTY;

        /* If there are no more reserved region to check then we can simply add
         * the whole available region (or the last chunk of it)
         */
        if (!is_active_reserved(idx_r)) {
            insert = *a;
            a_chunk = P_REG_EMPTY;
            idx_a++;
        }
        else
        {
            const p_region_t *r = &ndks_boot.reserved[idx_r];
            /* There can't be any invalid or empty region in the list. */
            assert(r->start < r->end);

            if (r->end <= a->start) {
                /* The reserved region is below the available region, skip it. */
                idx_r++;
            } else if (r->start >= a->end) {
                /* The reserved region is above the available region (or what is
                 * left of it), take it (or remaining chunk) as free memory.
                 */
                insert = *a;
                a_chunk = P_REG_EMPTY;
                idx_a++;
            } else if (r->start <= a->start) {
                /* The reserved region reaches into the available region. Skip the
                 * available region if it is fully within the reserved region,
                 * otherwise trim start of the available region to the end of the
                 * reserved region. */
                if (r->end >= a->end) {
                    a_chunk = P_REG_EMPTY;
                    idx_a++;
                } else {
                    a_chunk = (p_region_t) { .start = r->end, .end = a->end };
                    idx_r++;
                }
            } else if (r->start < a->end) {
                /* The reserved region starts inside the available region. Take the
                 * first chunk of the available region as free memory. Drop the rest
                 * of the the available region if it is fully within the reserved
                 * region, otherwise trim start of the available region to start at
                 * the end of the reserved resion.
                 */
                insert = (p_region_t){
                    .start = a->start,
                    .end   = r->start
                };
                if (a->end <= r->end) {
                    a_chunk = P_REG_EMPTY;
                    idx_a++;
                } else {
                    a_chunk = (p_region_t) { .start = r->end, .end = a->end };
                    idx_r++;
                }
            } else {
                /* We should never arrive here, as the check above are supposed to
                 * catch all possible conditions. If we are here, the algorithm must
                 * be broken. */
                assert(0);
                return cap_null_cap_new();
            }
        }

        /* Continue the loop if there is no free region that is to be added to
         * the list. Otherwise the region must be sane and not empty.
         */
        if (0 == insert.end) {
            continue;
        }

        printf("insert region [%"SEL4_PRIx_word"..%"SEL4_PRIx_word"] "
               "-> VA [%p..%p]\n",
               insert.start, insert.end - 1,
               paddr_to_pptr(insert.start),
               paddr_to_pptr(insert.end - 1));

        assert(insert.start < insert.end);

        /* Add region to the list of reserved regions. Rationale is, that now
         * that we know this area is used as free memory, we consider it as
         * reserved also. */
        if (!reserve_region(insert))
        {
            printf("ERROR: can't reserve free memory region "
                   "[%"SEL4_PRIx_word"..%"SEL4_PRIx_word"]\n",
                   insert.start, insert.end - 1);
            return cap_null_cap_new();
        }

        /* Reserving a region could modify the reserved regions list, so idx_r
         * is no longer valid. We have to start checking the reserved region
         * from the start. This is a simple solution and wont be expensive, most
         * likely we are going to skip a lot of reserved regions, because they
         * are before the current available region (or the chunk) we are
         * currently processing. And often, we skip less regions this time,
         * because some merged might have happened.
         */
        idx_r = 0;

        /* The kernel maps the physical memory completely in it's VA space,
         * which starts at PPTR_BASE. Unfortunately, VA space limitations could
         * prevent anything from the physical address space above PADDR_TOP to
         * be mapped, so there is no point in even adding it to the free memory
         * list. Usually, it's just an issue to worry about on 32-bit systems.
         */
        if (insert.end > PADDR_TOP) {
            if (insert.start >= PADDR_TOP) {
                printf("WARNING: can't use free memory region "
                       "[%"SEL4_PRIx_word"..%"SEL4_PRIx_word"], it is above "
                       "PADDR_TOP (%"SEL4_PRIx_word")\n",
                       insert.start, insert.end - 1, PADDR_TOP);
                /* We do not fail here, hopefully enough memory is be available
                 * below PADDR_TOP. If not the boot process will fail later
                 * anyway.
                 */
                continue;
            }
            /* Limit the memory area at PADDR_TOP. */
            printf("WARNING: Capping available free memory region "
                   "[%"SEL4_PRIx_word"..%"SEL4_PRIx_word"] at "
                   "PADDR_TOP (%"SEL4_PRIx_word")\n",
                   insert.start, insert.end - 1, PADDR_TOP);
            printf("         %"SEL4_PRIu_word" (0x%"SEL4_PRIx_word") byte of "
                   "physical memory are not accessible via the kernel window\n",
                   insert.end - PADDR_TOP, insert.end - PADDR_TOP);
            insert.end = PADDR_TOP;
        }

        /* Convert the physical address region to a virtual address region and
         * add it at the next free slot.
         */
        if (idx_f >= ARRAY_SIZE(ndks_boot.freemem))
        {
            printf("WARNING: can't add free memory region "
                   "[%"SEL4_PRIx_word"..%"SEL4_PRIx_word"], try increasing "
                   "MAX_NUM_FREEMEM_REG (currently %d)\n",
                   insert.start, insert.end - 1, (int)MAX_NUM_FREEMEM_REG);
            /* We do not fail here, hopefully enough memory is be available
             * already. If not the boot process will fail later anyway.
             */
            continue;
        }
        ndks_boot.freemem[idx_f] = insert;
        idx_f++;

    } // loop while (idx_a < n_available)

    /* Sanity check: idx_f can't exceed the number of array elements. */
    assert(idx_f <= ARRAY_SIZE(ndks_boot.freemem));
    printf("free mem setup done\n");
    printf("  reserved regions\n");
    for (word_t i = 0; is_active_reserved(i); i++) {
        p_region_t *r = &ndks_boot.reserved[i];
        printf("    [%"SEL4_PRIx_word"..%"SEL4_PRIx_word"]\n",
               r->start, r->end - 1);
        assert(r->start < r->end);
    }
    printf("  physical memory regions: %d\n", (int)n_available);
    for (word_t i = 0; i < n_available; i++) {
        const p_region_t* r = &available[i];
        printf("    [%"SEL4_PRIx_word"..%"SEL4_PRIx_word"]\n",
               r->start, r->end - 1 );
        assert(r->start < r->end);
    }
    printf("free mem:\n");
    for (word_t i = 0; i < ARRAY_SIZE(ndks_boot.freemem); i++) {
        p_region_t *r = &ndks_boot.freemem[i];
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

    /* Now ndks_boot.freemem is set up, so we can use it. First thing is
     * creating the rootserver objects in it.
     */
    printf("Setting up rootserver objects...\n");
    if (!create_rootserver_objects(it_v_reg, extra_bi_size_bits)) {
        printf("ERROR: could not create rootserver objects\n");
        return cap_null_cap_new();
    }

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
