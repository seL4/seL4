/*
 * Copyright 2016, Data61
 * Commonwealth Scientific and Industrial Research Organisation (CSIRO)
 * ABN 41 687 119 230.
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(D61_GPL)
 */

#ifndef __ARCH_MODE_OBJECT_STRUCTURES_H_
#define __ARCH_MODE_OBJECT_STRUCTURES_H_

/* x86-64 specific object types */
/* sysexit with rex.w prefix (64-bit) user code = cs + 32, user data = cs + 40.
 * without rex.w user code = cs + 16, user data = cs + 24, so we need to arrange
 * user CS and DS as 5 and 6.
 * */
#define GDT_NULL    0
#define GDT_CS_0    1
#define GDT_DS_0    2
#define GDT_TSS     3 //TSS is two slots in x86-64
#define GDT_CS_3    5
#define GDT_DS_3    6
#define GDT_TLS     7
#define GDT_IPCBUF  8
#define GDT_ENTRIES 9

compile_assert(gdt_idt_ptr_packed,
               sizeof(gdt_idt_ptr_t) == sizeof(uint16_t) * 5)

compile_assert(unsigned_long_size_64,
               sizeof(unsigned long) == 8)

compile_assert(unsinged_int_size_32,
               sizeof(unsigned int) == 4)

compile_assert(uint64_t_size_64,
               sizeof(uint64_t) == 8)

#define WORD_SIZE_BITS 3

#define X86_GLOBAL_VSPACE_ROOT x64KSGlobalPML4

#define PML4E_SIZE_BITS 3
#define PML4_INDEX_BITS 9
#define PDPTE_SIZE_BITS 3
#define PDPT_INDEX_BITS 9
#define PDE_SIZE_BITS   3
#define PD_INDEX_BITS   9
#define PTE_SIZE_BITS   3
#define PT_INDEX_BITS   9

#define PT_INDEX_OFFSET (seL4_PageBits)
#define PD_INDEX_OFFSET (PT_INDEX_OFFSET + PT_INDEX_BITS)
#define PDPT_INDEX_OFFSET (PD_INDEX_OFFSET + PD_INDEX_BITS)
#define PML4_INDEX_OFFSET (PDPT_INDEX_OFFSET + PDPT_INDEX_BITS)

typedef pml4e_t vspace_root_t;

#define GET_PML4_INDEX(x) ( ((x) >> (PML4_INDEX_OFFSET)) & MASK(PML4_INDEX_BITS))
#define GET_PDPT_INDEX(x) ( ((x) >> (PDPT_INDEX_OFFSET)) & MASK(PDPT_INDEX_BITS))
#define GET_PD_INDEX(x)   ( ((x) >> (PD_INDEX_OFFSET)) & MASK(PD_INDEX_BITS))
#define GET_PT_INDEX(x)   ( ((x) >> (PT_INDEX_OFFSET)) & MASK(PT_INDEX_BITS))

#define PML4E_PTR(r)     ((pml4e_t *)(r))
#define PML4E_PTR_PTR(r) ((pml4e_t **)(r))
#define PML4E_REF(p)     ((word_t)(p))

compile_assert(pml4_size_bits_sane, PML4_INDEX_BITS + PML4E_SIZE_BITS == seL4_PML4Bits)
#define PML4_PTR(r)     ((pml4e_t *)(r))
#define PML4_REF(p)     ((word_t)(r))

#define PDPTE_PTR(r)   ((pdpte_t *)(r))
#define PDPTE_PTR_PTR(r) ((pdpte_t **)(r))
#define PDPTE_REF(p)   ((word_t)(p))

compile_assert(pdpt_size_bits_sane, PDPT_INDEX_BITS + PDPTE_SIZE_BITS == seL4_PDPTBits)
#define PDPT_PTR(r)    ((pdpte_t *)(r))
#define PDPT_PREF(p)   ((word_t)(p))

#define PDE_PTR(r)     ((pde_t *)(r))
#define PDE_PTR_PTR(r) ((pde_t **)(r))
#define PDE_REF(p)     ((word_t)(p))

compile_assert(pd_size_bits_sane, PD_INDEX_BITS + PDE_SIZE_BITS == seL4_PageDirBits)
#define PD_PTR(r)    ((pde_t *)(r))
#define PD_REF(p)    ((word_t)(p))

#define PTE_PTR(r)    ((pte_t *)(r))
#define PTE_REF(p)    ((word_t)(p))

compile_assert(pt_size_bits_sane, PT_INDEX_BITS + PTE_SIZE_BITS == seL4_PageTableBits)
#define PT_PTR(r)    ((pte_t *)(r))
#define PT_REF(p)    ((word_t)(p))

/* there are 1^12 hardware PCID; now we match the software ASID
 * to the available PCID. Since each ASID pool is 4K in size,
 * it contains 512 vroots.
 */

enum asidSizeConstants {
    asidHighBits = 3,
    asidLowBits = 9
};

struct asid_pool {
    asid_map_t array[BIT(asidLowBits)];
};

typedef struct asid_pool asid_pool_t;

#define ASID_POOL_INDEX_BITS      asidLowBits
#define ASID_POOL_SIZE_BITS (ASID_POOL_BITS + WORD_SIZE_BITS)
#define ASID_POOL_PTR(r)    ((asid_pool_t*)r)
#define ASID_POOL_REF(p)    ((word_t)p)
#define ASID_BITS           (asidHighBits + asidLowBits)
#define nASIDPools          BIT(asidHighBits)
#define ASID_LOW(a)         (a & MASK(asidLowBits))
#define ASID_HIGH(a)        ((a >> asidLowBits) & MASK(asidHighBits))

static inline asid_t PURE
cap_get_capMappedASID(cap_t cap)
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

    default:
        fail("Invalid arch cap type");
    }
}

static inline word_t CONST
cap_get_modeCapSizeBits(cap_t cap)
{
    cap_tag_t ctag;

    ctag = cap_get_capType(cap);

    switch (ctag) {
    case cap_pml4_cap:
        return seL4_PML4Bits;

    default:
        fail("Invalid mode cap type");
    }
}

static inline bool_t CONST
cap_get_modeCapIsPhysical(cap_t cap)
{
    cap_tag_t ctag;

    ctag = cap_get_capType(cap);

    switch (ctag) {

    case cap_pml4_cap:
        return true;

    default:
        fail("Invalid mode cap type");
    }
}

static inline void * CONST
cap_get_modeCapPtr(cap_t cap)
{
    cap_tag_t ctag;

    ctag = cap_get_capType(cap);

    switch (ctag) {
    case cap_pml4_cap:
        return PML4_PTR(cap_pml4_cap_get_capPML4BasePtr(cap));

    default:
        fail("Invalid mode cap type");
    }
}

static inline pte_t
x86_make_device_pte(paddr_t phys)
{
    return pte_new(
               0,      /* xd */
               phys,   /* page_base_address    */
               1,      /* global               */
               0,      /* pat                  */
               0,      /* dirty                */
               0,      /* accessed             */
               1,      /* cache_disabled       */
               1,      /* write_through        */
               0,      /* super_user           */
               1,      /* read_write           */
               1       /* present              */
           );
}

static inline pte_t
x86_make_empty_pte(void)
{
    return pte_new(
               0,      /* xd */
               0,      /* page_base_address    */
               0,      /* global               */
               0,      /* pat                  */
               0,      /* dirty                */
               0,      /* accessed             */
               0,      /* cache_disabled       */
               0,      /* write_through        */
               0,      /* super_user           */
               0,      /* read_write           */
               0       /* present              */
           );
}

static inline pde_t
x86_make_pde_mapping(word_t paddr, vm_attributes_t attr)
{
    return pde_pde_large_new(
               0,
               paddr,
               vm_attributes_get_x86PATBit(attr),
               1,
               0,
               0,
               vm_attributes_get_x86PCDBit(attr),
               vm_attributes_get_x86PWTBit(attr),
               0,
               1,
               1
           );
}

#endif /* __ARCH_MODE_OBJECT_STRUCTURES_H_ */
