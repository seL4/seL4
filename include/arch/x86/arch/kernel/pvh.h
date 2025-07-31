/*
 * Copyright 2025, Heyang Zhou <hello@su3.io>
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#pragma once

#define HVM_START_MAGIC 0x336ec578

#include <types.h>
#include <sel4/arch/bootinfo_types.h>

#define XEN_HVM_MEMMAP_TYPE_RAM       1
#define XEN_HVM_MEMMAP_TYPE_RESERVED  2
#define XEN_HVM_MEMMAP_TYPE_ACPI      3
#define XEN_HVM_MEMMAP_TYPE_NVS       4
#define XEN_HVM_MEMMAP_TYPE_UNUSABLE  5
#define XEN_HVM_MEMMAP_TYPE_DISABLED  6
#define XEN_HVM_MEMMAP_TYPE_PMEM      7

typedef struct hvm_start_info {
    uint32_t magic;
    uint32_t version;
    uint32_t flags;
    uint32_t nr_modules;
    uint64_t modlist_paddr;
    uint64_t cmdline_paddr;
    uint64_t rsdp_paddr;
    uint64_t memmap_paddr;
    uint32_t memmap_entries;
    uint32_t reserved;
} hvm_start_info_t;

typedef struct hvm_memmap_entry {
    uint64_t addr;
    uint64_t size;
    uint32_t type;
    uint32_t reserved;
} hvm_memmap_entry_t;

typedef struct hvm_modlist_entry {
    uint64_t paddr;
    uint64_t size;
    uint64_t cmdline_paddr;
    uint64_t reserved;
} hvm_modlist_entry_t;
