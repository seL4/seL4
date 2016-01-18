/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(GD_GPL)
 */

#ifndef __ARCH_KERNEL_MULTIBOOT_H
#define __ARCH_KERNEL_MULTIBOOT_H

/* Adapted from the MultiBoot Specification:  */
/* www.gnu.org/software/grub/manual/multiboot */

#define MULTIBOOT_MAGIC 0x2BADB002

#include <types.h>

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
    uint32_t flags;
    uint32_t mem_lower;
    uint32_t mem_upper;
    uint32_t boot_device;
    uint32_t cmdline;
    uint32_t mod_count;
    uint32_t mod_list;
    /* The symbol table information in the multiboot header is comprised of a union
     * as we neither a. support unions in the kernel or b. need the symbol information
     * we will just skip the 4 words of this */
    uint32_t syms[4];
    uint32_t mmap_length;
    uint32_t mmap_addr;
    /* the multiboot spec includes more fields we don't need */
} PACKED multiboot_info_t;

#define MULTIBOOT_INFO_MEM_FLAG     BIT(0)
#define MULTIBOOT_INFO_CMDLINE_FLAG BIT(2)
#define MULTIBOOT_INFO_MODS_FLAG    BIT(3)
#define MULTIBOOT_INFO_MMAP_FLAG    BIT(6)
#define MULTIBOOT_MMAP_USEABLE_TYPE     1
#define MULTIBOOT_MMAP_RESERVED_TYPE    2
#define MULTIBOOT_MMAP_ACPI_TYPE        3
#define MULTIBOOT_MMAP_ACPI_NVS_TYPE    4
#define MULTIBOOT_MMAP_BAD_TYPE         5

#endif
