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
#include <arch/kernel/multiboot.h>
#include <arch/kernel/bootinfo.h>
#include <plat/machine/acpi.h>

typedef struct dev_p_regs {
    word_t count;
    p_region_t list[10];
} dev_p_regs_t;

typedef struct ui_info {
    p_region_t p_reg;     /* region where the userland image lies in */
    int32_t    pv_offset; /* UI virtual address + pv_offset = UI physical address */
    vptr_t     v_entry;   /* entry point (virtual address) of userland image */
} ui_info_t;

typedef struct vesa_info {
    vbe_control_info_t vbe_control_info;
    vbe_mode_info_t vbe_mode_info;
    uint32_t vbe_mode;
    uint32_t vbe_interface_seg;
    uint32_t vbe_interface_off;
    uint32_t vbe_interface_len;
} vesa_info_t;

cap_t create_unmapped_it_frame_cap(pptr_t pptr, bool_t use_large);
cap_t create_mapped_it_frame_cap(cap_t pd_cap, pptr_t pptr, vptr_t vptr, bool_t use_large, bool_t executable);

bool_t init_node_state(
    p_region_t    avail_p_reg,
    p_region_t    sh_p_reg,
    dev_p_regs_t* dev_p_regs,
    ui_info_t     ui_info,
    p_region_t    boot_mem_reuse_p_reg,
    node_id_t     node_id,
    uint32_t      num_nodes,
    cpu_id_t*     cpu_list,
    /* parameters below not modeled in abstract specification */
    pdpte_t*      kernel_pdpt,
    pde_t*        kernel_pd,
    pte_t*        kernel_pt,
    vesa_info_t*  vesa_info,
    ia32_mem_region_t* mem_regions
#ifdef CONFIG_IOMMU
    , cpu_id_t      cpu_id,
    uint32_t      num_drhu,
    paddr_t*      drhu_list,
    acpi_rmrr_list_t *rmrr_list
#endif
);

bool_t init_node_cpu(
    uint32_t apic_khz,
    bool_t   mask_legacy_irqs
);

#endif
