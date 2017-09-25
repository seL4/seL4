/*
 * Copyright 2017, Genode Labs GmbH
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 */

#ifndef __ARCH_KERNEL_MULTIBOOT2_H
#define __ARCH_KERNEL_MULTIBOOT2_H

#define MULTIBOOT2_MAGIC 0x36d76289

#include <types.h>

typedef struct multiboot2_header {
    uint32_t total_size;
    uint32_t unknown;
} PACKED multiboot2_header_t;

typedef struct multiboot2_tag {
    uint32_t type;
    uint32_t size;
} PACKED multiboot2_tag_t;

typedef struct multiboot2_memory {
    uint64_t addr;
    uint64_t size;
    uint32_t type;
    uint32_t reserved;
} PACKED multiboot2_memory_t;

typedef struct multiboot2_module {
    uint32_t start;
    uint32_t end;
    char     string [0];
} PACKED multiboot2_module_t;

enum multiboot2_tags {
    MULTIBOOT2_TAG_END     = 0,
    MULTIBOOT2_TAG_CMDLINE = 1,
    MULTIBOOT2_TAG_MODULE  = 3,
    MULTIBOOT2_TAG_MEMORY  = 6,
    MULTIBOOT2_TAG_ACPI    = 15,
};

#endif
