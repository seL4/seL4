/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#pragma once

#include <config.h>
#include <types.h>
#include <bootinfo.h>
#include <arch/bootinfo.h>

/*
 * Resolve naming differences between the abstract specifications
 * of the bootstrapping phase and the runtime phase of the kernel.
 */
typedef cte_t  slot_t;
typedef cte_t *slot_ptr_t;
#define SLOT_PTR(pptr, pos) (((slot_ptr_t)(pptr)) + (pos))
#define pptr_of_cap(cap) ((pptr_t)cap_get_capPtr(cap))

/* (node-local) state accessed only during bootstrapping */

typedef struct ndks_boot {
    p_region_t reserved[MAX_NUM_RESV_REG];
    word_t resv_count;
    region_t   freemem[MAX_NUM_FREEMEM_REG];
    seL4_BootInfo      *bi_frame;
    seL4_SlotPos slot_pos_cur;
} ndks_boot_t;

extern ndks_boot_t ndks_boot;

/* function prototypes */

static inline bool_t is_reg_empty(region_t reg)
{
    return reg.start == reg.end;
}

p_region_t get_p_reg_kernel_img_boot(void);
p_region_t get_p_reg_kernel_img(void);
bool_t init_freemem(word_t n_available, const p_region_t *available,
                    word_t n_reserved, const region_t *reserved,
                    v_region_t it_v_reg, word_t extra_bi_size_bits);
bool_t reserve_region(p_region_t reg);
void write_slot(slot_ptr_t slot_ptr, cap_t cap);
cap_t create_root_cnode(void);
bool_t provide_cap(cap_t root_cnode_cap, cap_t cap);
cap_t create_it_asid_pool(cap_t root_cnode_cap);
void write_it_pd_pts(cap_t root_cnode_cap, cap_t it_pd_cap);
void create_idle_thread(void);
bool_t create_untypeds(cap_t root_cnode_cap);
void bi_finalise(void);
void create_domain_cap(cap_t root_cnode_cap);

cap_t create_ipcbuf_frame_cap(cap_t root_cnode_cap, cap_t pd_cap, vptr_t vptr);
word_t calculate_extra_bi_size_bits(word_t extra_size);
void populate_bi_frame(node_id_t node_id, word_t num_nodes, vptr_t ipcbuf_vptr,
                       word_t extra_bi_size_bits);
void create_bi_frame_cap(cap_t root_cnode_cap, cap_t pd_cap, vptr_t vptr);

#ifdef CONFIG_KERNEL_MCS
bool_t init_sched_control(cap_t root_cnode_cap, word_t num_nodes);
#endif

typedef struct create_frames_of_region_ret {
    seL4_SlotRegion region;
    bool_t success;
} create_frames_of_region_ret_t;

create_frames_of_region_ret_t
create_frames_of_region(
    cap_t    root_cnode_cap,
    cap_t    pd_cap,
    region_t reg,
    bool_t   do_map,
    sword_t  pv_offset
);

cap_t
create_it_pd_pts(
    cap_t      root_cnode_cap,
    v_region_t ui_v_reg,
    vptr_t     ipcbuf_vptr,
    vptr_t     bi_frame_vptr
);

tcb_t *
create_initial_thread(
    cap_t  root_cnode_cap,
    cap_t  it_pd_cap,
    vptr_t ui_v_entry,
    vptr_t bi_frame_vptr,
    vptr_t ipcbuf_vptr,
    cap_t  ipcbuf_cap
);

void init_core_state(tcb_t *scheduler_action);

/* state tracking the memory allocated for root server objects */
typedef struct {
    pptr_t cnode;
    pptr_t vspace;
    pptr_t asid_pool;
    pptr_t ipc_buf;
    pptr_t boot_info;
    pptr_t extra_bi;
    pptr_t tcb;
#ifdef CONFIG_KERNEL_MCS
    pptr_t sc;
#endif
    region_t paging;
} rootserver_mem_t;

extern rootserver_mem_t rootserver;

/* get the number of paging structures required to cover it_v_reg, with
 * the paging structure covering `bits` of the address range - for a 4k page
 * `bits` would be 12 */
static inline BOOT_CODE word_t get_n_paging(v_region_t v_reg, word_t bits)
{
    vptr_t start = ROUND_DOWN(v_reg.start, bits);
    vptr_t end = ROUND_UP(v_reg.end, bits);
    return (end - start) / BIT(bits);
}

/* allocate a page table sized structure from rootserver.paging */
static inline BOOT_CODE pptr_t it_alloc_paging(void)
{
    pptr_t allocated = rootserver.paging.start;
    rootserver.paging.start += BIT(seL4_PageTableBits);
    assert(rootserver.paging.start <= rootserver.paging.end);
    return allocated;
}

/* return the amount of paging structures required to cover v_reg */
word_t arch_get_n_paging(v_region_t it_veg);

#if defined(CONFIG_DEBUG_BUILD) && defined(ENABLE_SMP_SUPPORT) && defined(CONFIG_KERNEL_MCS)
/* Test whether clocks are synchronised across nodes */
#define ENABLE_SMP_CLOCK_SYNC_TEST_ON_BOOT
#endif

#ifdef ENABLE_SMP_CLOCK_SYNC_TEST_ON_BOOT
BOOT_CODE void clock_sync_test(void);
#else
#define clock_sync_test()
#endif
