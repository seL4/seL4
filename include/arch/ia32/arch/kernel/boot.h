/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(GD_GPL)
 */

#ifndef __ARCH_KERNEL_BOOT_NODE_H
#define __ARCH_KERNEL_BOOT_NODE_H

#include <config.h>
#include <types.h>

typedef struct dev_p_regs {
    uint32_t count;
    p_region_t list[CONFIG_MAX_NUM_BOOTINFO_DEVICE_REGIONS];
} dev_p_regs_t;

typedef struct ui_info {
    p_region_t p_reg;     /* region where the userland image lies in */
    int32_t    pv_offset; /* UI virtual address + pv_offset = UI physical address */
    vptr_t     v_entry;   /* entry point (virtual address) of userland image */
} ui_info_t;

cap_t create_unmapped_it_frame_cap(pptr_t pptr, bool_t use_large);
cap_t create_mapped_it_frame_cap(cap_t pd_cap, pptr_t pptr, vptr_t vptr, asid_t asid, bool_t use_large);

bool_t init_node_state(
    p_region_t    avail_p_reg,
    p_region_t    sh_p_reg,
    dev_p_regs_t* dev_p_regs,
    ui_info_t     ui_info,
    p_region_t    boot_mem_reuse_p_reg,
    node_id_t     node_id,
    uint32_t      num_nodes,
    /* parameters below not modeled in abstract specification */
    pde_t*        kernel_pd,
    pte_t*        kernel_pt
#ifdef CONFIG_IOMMU
    , cpu_id_t      cpu_id,
    uint32_t      num_drhu,
    paddr_t*      drhu_list,
    uint32_t      num_passthrough_dev,
    dev_id_t*     passthrough_dev_list,
    uint32_t*     pci_bus_used_bitmap
#endif
);

bool_t init_node_cpu(
    uint32_t apic_khz,
    bool_t   mask_legacy_irqs
);

#endif
