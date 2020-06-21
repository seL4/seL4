/*
 * Copyright 2017, Genode Labs GmbH
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#pragma once

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
    char     string [1];
} PACKED multiboot2_module_t;

typedef struct multiboot2_fb {
    uint64_t addr;
    uint32_t pitch;
    uint32_t width;
    uint32_t height;
    uint8_t  bpp;
    uint8_t  type;
} PACKED multiboot2_fb_t;

enum multiboot2_tags {
    MULTIBOOT2_TAG_END     = 0,
    MULTIBOOT2_TAG_CMDLINE = 1,
    MULTIBOOT2_TAG_MODULE  = 3,
    MULTIBOOT2_TAG_MEMORY  = 6,
    MULTIBOOT2_TAG_FB      = 8,
    MULTIBOOT2_TAG_ACPI_1  = 14,
    MULTIBOOT2_TAG_ACPI_2  = 15,
};

