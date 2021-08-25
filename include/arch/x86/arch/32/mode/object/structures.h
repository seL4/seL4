/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#pragma once

#include <config.h>
#include <sel4/macros.h>

#define GDT_NULL    0
#define GDT_CS_0    1
#define GDT_DS_0    2
#define GDT_CS_3    3
#define GDT_DS_3    4
#define GDT_TSS     5
#define GDT_FS      6
#define GDT_GS      7
#define GDT_ENTRIES 8

#define PDE_SIZE_BITS seL4_PageDirEntryBits
#define PD_INDEX_BITS seL4_PageDirIndexBits
#define PTE_SIZE_BITS seL4_PageTableEntryBits
#define PT_INDEX_BITS seL4_PageTableIndexBits
#define X86_GLOBAL_VSPACE_ROOT ia32KSGlobalPD
#define X86_KERNEL_VSPACE_ROOT ia32KSGlobalPD
typedef pde_t vspace_root_t;

#define GET_VSPACE_ROOT_INDEX(x) ((x) >> (seL4_PageBits + PT_INDEX_BITS))

#define PDE_PTR(r)     ((pde_t *)(r))
#define PDE_PTR_PTR(r) ((pde_t **)(r))
#define PDE_REF(p)     ((word_t)(p))

#define PD_PTR(r)    ((pde_t *)(r))
#define PD_REF(p)    ((word_t)(p))

#define PTE_PTR(r)    ((pte_t *)(r))
#define PTE_REF(p)    ((word_t)(p))

#define PT_PTR(r)    ((pte_t *)(r))
#define PT_REF(p)    ((word_t)(p))

compile_assert(gdt_idt_ptr_packed,
               sizeof(gdt_idt_ptr_t) == sizeof(uint16_t) * 3)

struct asid_pool {
    asid_map_t array[BIT(asidLowBits)];
};

typedef struct asid_pool asid_pool_t;

#define ASID_POOL_INDEX_BITS    seL4_ASIDPoolIndexBits
#define ASID_POOL_PTR(r)    ((asid_pool_t*)r)
#define ASID_POOL_REF(p)    ((word_t)p)
#define ASID_BITS           (asidHighBits + asidLowBits)
#define nASIDPools          BIT(asidHighBits)
#define ASID_LOW(a)         (a & MASK(asidLowBits))
#define ASID_HIGH(a)        ((a >> asidLowBits) & MASK(asidHighBits))

static inline asid_t CONST cap_frame_cap_get_capFMappedASID(cap_t cap)
{
    return
        (cap_frame_cap_get_capFMappedASIDHigh(cap) << asidLowBits) +
        cap_frame_cap_get_capFMappedASIDLow(cap);
}

static inline cap_t CONST cap_frame_cap_set_capFMappedASID(cap_t cap, word_t asid)
{
    cap = cap_frame_cap_set_capFMappedASIDLow(cap, ASID_LOW(asid));
    return cap_frame_cap_set_capFMappedASIDHigh(cap, ASID_HIGH(asid));
}

static inline asid_t PURE cap_frame_cap_ptr_get_capFMappedASID(cap_t *cap)
{
    return cap_frame_cap_get_capFMappedASID(*cap);
}

static inline void cap_frame_cap_ptr_set_capFMappedASID(cap_t *cap, asid_t asid)
{
    *cap = cap_frame_cap_set_capFMappedASID(*cap, asid);
}

static inline asid_t PURE cap_get_capMappedASID(cap_t cap)
{
    cap_tag_t ctag;

    ctag = cap_get_capType(cap);

    switch (ctag) {
    case cap_page_directory_cap:
        return cap_page_directory_cap_get_capPDMappedASID(cap);

#ifdef CONFIG_VTX
    case cap_ept_pml4_cap:
        return cap_ept_pml4_cap_get_capPML4MappedASID(cap);
#endif

    default:
        fail("Invalid arch cap type");
    }
}

static inline word_t CONST cap_get_modeCapSizeBits(cap_t cap)
{
    fail("Invalid mode cap type");
}

static inline bool_t CONST cap_get_modeCapIsPhysical(cap_t cap)
{
    fail("Invalid mode cap type");
}

static inline void *CONST cap_get_modeCapPtr(cap_t cap)
{
    fail("Invalid mode cap type");
}

