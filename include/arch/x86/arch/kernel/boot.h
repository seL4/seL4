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
#include <plat/machine/acpi.h>
#include <kernel/boot.h>
#include <arch/api/bootinfo_types.h>

typedef struct mem_p_regs {
    word_t count;
    p_region_t list[MAX_NUM_FREEMEM_REG];
} mem_p_regs_t;

typedef struct ui_info {
    p_region_t p_reg;     /* region where the userland image lies in */
    sword_t    pv_offset; /* UI virtual address + pv_offset = UI physical address */
    vptr_t     v_entry;   /* entry point (virtual address) of userland image */
} ui_info_t;

cap_t create_unmapped_it_frame_cap(pptr_t pptr, bool_t use_large);
cap_t create_mapped_it_frame_cap(cap_t pd_cap, pptr_t pptr, vptr_t vptr, asid_t asid, bool_t use_large, bool_t executable);

bool_t init_sys_state(
    cpu_id_t      cpu_id,
    mem_p_regs_t  mem_p_regs,
    ui_info_t     ui_info,
    p_region_t    boot_mem_reuse_p_reg,
    /* parameters below not modeled in abstract specification */
    uint32_t      num_drhu,
    paddr_t*      drhu_list,
    acpi_rmrr_list_t *rmrr_list,
    acpi_rsdp_t      *acpi_rsdp,
    seL4_X86_BootInfo_VBE *vbe,
    seL4_X86_BootInfo_mmap_t *mb_mmap
);

bool_t init_cpu(
    bool_t   mask_legacy_irqs
);

bool_t add_allocated_p_region(p_region_t reg);
void init_allocated_p_regions(void);

#endif
