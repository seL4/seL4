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
    paddr_t  start;
    paddr_t  end;
    char*    name;
    uint32_t reserved;
} multiboot_module_t;

typedef struct multiboot_info {
    uint32_t flags;
    uint32_t mem_lower;
    uint32_t mem_upper;
    uint32_t boot_device;
    char*    cmdline;
    uint32_t mod_count;
    multiboot_module_t* mod_list;
    /* the multiboot spec includes more fields we don't need */
} multiboot_info_t;

#define MULTIBOOT_INFO_MEM_FLAG     BIT(0)
#define MULTIBOOT_INFO_CMDLINE_FLAG BIT(2)
#define MULTIBOOT_INFO_MODS_FLAG    BIT(3)

#endif
