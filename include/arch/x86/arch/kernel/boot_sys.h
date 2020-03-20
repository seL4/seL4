/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#pragma once

#include <arch/kernel/multiboot.h>
#include <arch/kernel/multiboot2.h>

/* type definitions (directly corresponding to abstract specification) */
typedef struct boot_state {
    p_region_t   avail_p_reg; /* region of available physical memory on platform */
    p_region_t   ki_p_reg;    /* region where the kernel image is in */
    ui_info_t    ui_info;     /* info about userland images */
    uint32_t     num_ioapic;  /* number of IOAPICs detected */
    paddr_t      ioapic_paddr[CONFIG_MAX_NUM_IOAPIC];
    uint32_t     num_drhu; /* number of IOMMUs */
    paddr_t      drhu_list[MAX_NUM_DRHU]; /* list of physical addresses of the IOMMUs */
    acpi_rmrr_list_t rmrr_list;
    acpi_rsdp_t  acpi_rsdp; /* copy of the rsdp */
    paddr_t      mods_end_paddr; /* physical address where boot modules end */
    paddr_t      boot_module_start; /* physical address of first boot module */
    uint32_t     num_cpus;    /* number of detected cpus */
    uint32_t     mem_lower;   /* lower memory size for boot code of APs to run in real mode */
    cpu_id_t     cpus[CONFIG_MAX_NUM_NODES];
    mem_p_regs_t mem_p_regs;  /* physical memory regions */
    seL4_X86_BootInfo_VBE vbe_info; /* Potential VBE information from multiboot */
    seL4_X86_BootInfo_mmap_t mb_mmap_info; /* memory map information from multiboot */
    seL4_X86_BootInfo_fb_t fb_info; /* framebuffer information as set by bootloader */
} boot_state_t;

extern boot_state_t boot_state;

void boot_sys(
    unsigned long multiboot_magic,
    void *multiboot
);

