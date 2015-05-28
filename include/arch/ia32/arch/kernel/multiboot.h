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

typedef struct vbe_control_info {
    /* We do not need to interpret the contents, just get the size right */
    uint32_t data[512 / sizeof(uint32_t)];
} vbe_control_info_t;

typedef struct vbe_mode_info {
    /* We do not need to interpret the contents, just get the size right */
    uint32_t data[256 / sizeof(uint32_t)];
} vbe_mode_info_t;

typedef struct multiboot_mmap_entry {
    uint32_t size;
    uint64_t addr;
    uint64_t len;
    uint32_t type;
} __attribute__((packed)) multiboot_memory_map_t;

typedef struct multiboot_info {
    uint32_t flags;
    uint32_t mem_lower;
    uint32_t mem_upper;
    uint32_t boot_device;
    uint32_t cmdline;
    uint32_t mod_count;
    uint32_t mod_list;
    uint32_t syms[4];
    uint32_t mmap_length;
    void *mmap_addr;
    uint32_t drives_length;
    void *drives_addr;
    void *config_table;
    void *boot_loader_name;
    void *apm_table;
    vbe_control_info_t *vbe_control_info;
    vbe_mode_info_t *vbe_mode_info;
    uint16_t vbe_mode;
    uint16_t vbe_interface_seg;
    uint16_t vbe_interface_off;
    uint16_t vbe_interface_len;
} __attribute__((packed)) multiboot_info_t;

#define MULTIBOOT_INFO_MEM_FLAG     BIT(0)
#define MULTIBOOT_INFO_CMDLINE_FLAG BIT(2)
#define MULTIBOOT_INFO_MODS_FLAG    BIT(3)
#define MULTIBOOT_INFO_MEM_MAP      BIT(6)
#define MULTIBOOT_INFO_GRAPHICS_FLAG BIT(11)

#define MULTIBOOT_MEMORY_AVAILABLE              1
#define MULTIBOOT_MEMORY_RESERVED               2

#endif
