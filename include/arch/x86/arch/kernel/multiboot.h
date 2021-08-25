/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#pragma once
/* Adapted from the MultiBoot Specification:  */
/* www.gnu.org/software/grub/manual/multiboot */

#define MULTIBOOT_MAGIC 0x2BADB002

#include <types.h>
#include <sel4/arch/bootinfo_types.h>

typedef struct multiboot_module {
    uint32_t  start;
    uint32_t  end;
    uint32_t  name;
    uint32_t reserved;
} PACKED multiboot_module_t;

typedef struct multiboot_mmap {
    uint32_t size;
    uint64_t base_addr;
    uint64_t length;
    uint32_t type;
} PACKED multiboot_mmap_t;

typedef struct multiboot_info {
    /* struct split into multiple parts due to details of how C parser works */
    struct multiboot_part1 {
        uint32_t flags;
        uint32_t mem_lower;
        uint32_t mem_upper;
        uint32_t boot_device;
        uint32_t cmdline;
        uint32_t mod_count;
        uint32_t mod_list;
    } part1;
    /* The symbol table information in the multiboot header is comprised of a union
     * as we neither a. support unions in the kernel or b. need the symbol information
     * we will just skip the 4 words of this */
    struct multiboot_part2 {
        uint32_t syms[4];
        uint32_t mmap_length;
        uint32_t mmap_addr;
        uint32_t drives_length;
        uint32_t drives_addr;
        uint32_t config_table;
        uint32_t boot_loader_name;
        uint32_t apm_table;
        uint32_t vbe_control_info;
        uint32_t vbe_mode_info;
        uint16_t vbe_mode;
        uint16_t vbe_interface_seg;
        uint16_t vbe_interface_off;
        uint16_t vbe_interface_len;
    } part2;
} PACKED multiboot_info_t;

#define MULTIBOOT_INFO_MEM_FLAG     BIT(0)
#define MULTIBOOT_INFO_CMDLINE_FLAG BIT(2)
#define MULTIBOOT_INFO_MODS_FLAG    BIT(3)
#define MULTIBOOT_INFO_MMAP_FLAG    BIT(6)
#define MULTIBOOT_INFO_GRAPHICS_FLAG BIT(11)
#define MULTIBOOT_MMAP_USEABLE_TYPE     1
#define MULTIBOOT_MMAP_RESERVED_TYPE    2
#define MULTIBOOT_MMAP_ACPI_TYPE        3
#define MULTIBOOT_MMAP_ACPI_NVS_TYPE    4
#define MULTIBOOT_MMAP_BAD_TYPE         5

