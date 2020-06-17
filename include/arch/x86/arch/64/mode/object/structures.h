/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#pragma once

#include <sel4/macros.h>
/* x86-64 specific object types */
/* sysexitq (64-bit) user code = cs + 32, user data = cs + 40.
 * sysexit user code = cs + 16, user data = cs + 24, so we need to arrange
 * user CS and DS as 5 and 6.
 * */
#define GDT_NULL    0
#define GDT_CS_0    1
#define GDT_DS_0    2
#define GDT_TSS     3 //TSS is two slots in x86-64
#define GDT_CS_3    5
#define GDT_DS_3    6
#define GDT_FS      7
#define GDT_GS      8
#define GDT_ENTRIES 9

compile_assert(gdt_idt_ptr_packed,
               sizeof(gdt_idt_ptr_t) == sizeof(uint16_t) * 5)

compile_assert(unsigned_long_size_64,
               sizeof(unsigned long) == 8)

compile_assert(unsinged_int_size_32,
               sizeof(unsigned int) == 4)

compile_assert(uint64_t_size_64,
               sizeof(uint64_t) == 8)

#ifdef CONFIG_KERNEL_SKIM_WINDOW
#define X86_GLOBAL_VSPACE_ROOT x64KSSKIMPML4
#else
#define X86_GLOBAL_VSPACE_ROOT x64KSKernelPML4
#endif

#define X86_KERNEL_VSPACE_ROOT x64KSKernelPML4

#define PML4E_SIZE_BITS seL4_PML4EntryBits
#define PML4_INDEX_BITS seL4_PML4IndexBits
#define PDPTE_SIZE_BITS seL4_PDPTEntryBits
#define PDPT_INDEX_BITS seL4_PDPTIndexBits
#define PDE_SIZE_BITS   seL4_PageDirEntryBits
#define PD_INDEX_BITS   seL4_PageDirIndexBits
#define PTE_SIZE_BITS   seL4_PageTableEntryBits
#define PT_INDEX_BITS   seL4_PageTableIndexBits

#define PT_INDEX_OFFSET (seL4_PageBits)
#define PD_INDEX_OFFSET (PT_INDEX_OFFSET + PT_INDEX_BITS)
#define PDPT_INDEX_OFFSET (PD_INDEX_OFFSET + PD_INDEX_BITS)
#define PML4_INDEX_OFFSET (PDPT_INDEX_OFFSET + PDPT_INDEX_BITS)

typedef pml4e_t vspace_root_t;

#define GET_PML4_INDEX(x) ( ((x) >> (PML4_INDEX_OFFSET)) & MASK(PML4_INDEX_BITS))
#define GET_VSPACE_ROOT_INDEX GET_PML4_INDEX
#define GET_PDPT_INDEX(x) ( ((x) >> (PDPT_INDEX_OFFSET)) & MASK(PDPT_INDEX_BITS))
#define GET_PD_INDEX(x)   ( ((x) >> (PD_INDEX_OFFSET)) & MASK(PD_INDEX_BITS))
#define GET_PT_INDEX(x)   ( ((x) >> (PT_INDEX_OFFSET)) & MASK(PT_INDEX_BITS))

#define PML4E_PTR(r)     ((pml4e_t *)(r))
#define PML4E_PTR_PTR(r) ((pml4e_t **)(r))
#define PML4E_REF(p)     ((word_t)(p))

#define PML4_PTR(r)     ((pml4e_t *)(r))
#define PML4_REF(p)     ((word_t)(r))

#define PDPTE_PTR(r)   ((pdpte_t *)(r))
#define PDPTE_PTR_PTR(r) ((pdpte_t **)(r))
#define PDPTE_REF(p)   ((word_t)(p))

#define PDPT_PTR(r)    ((pdpte_t *)(r))
#define PDPT_PREF(p)   ((word_t)(p))

#define PDE_PTR(r)     ((pde_t *)(r))
#define PDE_PTR_PTR(r) ((pde_t **)(r))
#define PDE_REF(p)     ((word_t)(p))

#define PD_PTR(r)    ((pde_t *)(r))
#define PD_REF(p)    ((word_t)(p))

#define PTE_PTR(r)    ((pte_t *)(r))
#define PTE_REF(p)    ((word_t)(p))

#define PT_PTR(r)    ((pte_t *)(r))
#define PT_REF(p)    ((word_t)(p))

/* there are 1^12 hardware PCID; now we match the software ASID
 * to the available PCID. Since each ASID pool is 4K in size,
 * it contains 512 vroots.
 */

struct asid_pool {
    asid_map_t array[BIT(asidLowBits)];
};

typedef struct asid_pool asid_pool_t;

#define ASID_POOL_INDEX_BITS  seL4_ASIDPoolIndexBits
#define ASID_POOL_SIZE_BITS (seL4_ASIDPoolBits + WORD_SIZE_BITS)
#define ASID_POOL_PTR(r)    ((asid_pool_t*)r)
#define ASID_POOL_REF(p)    ((word_t)p)
#define ASID_BITS           (asidHighBits + asidLowBits)
#define nASIDPools          BIT(asidHighBits)
#define ASID_LOW(a)         (a & MASK(asidLowBits))
#define ASID_HIGH(a)        ((a >> asidLowBits) & MASK(asidHighBits))

static inline asid_t PURE cap_get_capMappedASID(cap_t cap)
{
    cap_tag_t ctag;

    ctag = cap_get_capType(cap);

    switch (ctag) {

    case cap_pml4_cap:
        return cap_pml4_cap_get_capPML4MappedASID(cap);

    case cap_pdpt_cap:
        return cap_pdpt_cap_get_capPDPTMappedASID(cap);

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
    cap_tag_t ctag;

    ctag = cap_get_capType(cap);

    switch (ctag) {
    case cap_pml4_cap:
        return seL4_PML4Bits;

    case cap_pdpt_cap:
        return seL4_PDPTBits;

    default:
        return 0;
    }
}

static inline bool_t CONST cap_get_modeCapIsPhysical(cap_t cap)
{
    cap_tag_t ctag;

    ctag = cap_get_capType(cap);

    switch (ctag) {

    case cap_pml4_cap:
        return true;

    case cap_pdpt_cap:
        return true;

    default:
        return false;
    }
}

static inline void *CONST cap_get_modeCapPtr(cap_t cap)
{
    cap_tag_t ctag;

    ctag = cap_get_capType(cap);

    switch (ctag) {
    case cap_pml4_cap:
        return PML4_PTR(cap_pml4_cap_get_capPML4BasePtr(cap));

    case cap_pdpt_cap:
        return PDPT_PTR(cap_pdpt_cap_get_capPDPTBasePtr(cap));

    default:
        return NULL;
    }
}

