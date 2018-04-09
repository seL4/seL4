/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(GD_GPL)
 */

#ifndef __KERNEL_BOOT_H
#define __KERNEL_BOOT_H

#include <bootinfo.h>

#ifndef CONFIG_ARCH_ARM
#define MAX_NUM_FREEMEM_REG 16
#else
#define MAX_NUM_FREEMEM_REG 2
#endif

/*
 * Resolve naming differences between the abstract specifications
 * of the bootstrapping phase and the runtime phase of the kernel.
 */
typedef cte_t  slot_t;
typedef cte_t* slot_ptr_t;
#define SLOT_PTR(pptr, pos) (((slot_ptr_t)(pptr)) + (pos))
#define pptr_of_cap (pptr_t)cap_get_capPtr

/* (node-local) state accessed only during bootstrapping */

typedef struct ndks_boot {
    region_t   freemem[MAX_NUM_FREEMEM_REG];
    seL4_BootInfo*      bi_frame;
    seL4_SlotPos slot_pos_cur;
    seL4_SlotPos slot_pos_max;
} ndks_boot_t;

extern ndks_boot_t ndks_boot;

/* function prototypes */

static inline bool_t
is_reg_empty(region_t reg)
{
    return reg.start == reg.end;
}

pptr_t alloc_region(word_t size_bits);
bool_t insert_region(region_t reg);
void write_slot(slot_ptr_t slot_ptr, cap_t cap);
cap_t create_root_cnode(void);
bool_t provide_cap(cap_t root_cnode_cap, cap_t cap);
cap_t create_it_asid_pool(cap_t root_cnode_cap);
void write_it_pd_pts(cap_t root_cnode_cap, cap_t it_pd_cap);
bool_t create_idle_thread(void);
bool_t create_untypeds_for_region(cap_t root_cnode_cap, bool_t device_memory, region_t reg, seL4_SlotPos first_untyped_slot);
bool_t create_kernel_untypeds(cap_t root_cnode_cap, region_t boot_mem_reuse_reg, seL4_SlotPos first_untyped_slot);
void bi_finalise(void);
bool_t create_irq_cnode(void);
void create_domain_cap(cap_t root_cnode_cap);

cap_t create_ipcbuf_frame(cap_t root_cnode_cap, cap_t pd_cap, vptr_t vptr);

region_t allocate_extra_bi_region(word_t extra_size);
pptr_t allocate_bi_frame(node_id_t node_id, word_t num_nodes, vptr_t ipcbuf_vptr);

void create_bi_frame_cap(cap_t root_cnode_cap, cap_t pd_cap, pptr_t pptr, vptr_t vptr);

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
#endif
