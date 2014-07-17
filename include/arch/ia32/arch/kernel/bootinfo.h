/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(GD_GPL)
 */

#ifndef __ARCH_KERNEL_BOOTINFO_H
#define __ARCH_KERNEL_BOOTINFO_H

#include "multiboot.h"

typedef struct ia32_mem_region {
    uint64_t paddr;
    uint64_t len;
} ia32_mem_region_t;

typedef struct ia32_bootinfo_frame {
    vbe_control_info_t vbe_control_info;
    vbe_mode_info_t vbe_mode_info;
    uint32_t vbe_mode;
    uint32_t vbe_interface_seg;
    uint32_t vbe_interface_off;
    uint32_t vbe_interface_len;
    ia32_mem_region_t mem_regions[CONFIG_MAX_MEM_REGIONS];
} ia32_bootinfo_frame_t;

compile_assert(ia32_bootinfo_frame_4k, sizeof(ia32_bootinfo_frame_t) < BIT(PAGE_BITS));

#endif
