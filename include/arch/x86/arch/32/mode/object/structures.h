/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(GD_GPL)
 */

#ifndef __MODE_OBJECT_STRUCTURES_H
#define __MODE_OBJECT_STRUCTURES_H

/* update this when you modify the tcb struct */
#define EXPECTED_TCB_SIZE 690

#define GDT_NULL    0
#define GDT_CS_0    1
#define GDT_DS_0    2
#define GDT_CS_3    3
#define GDT_DS_3    4
#define GDT_TSS     5
#define GDT_TLS     6
#define GDT_IPCBUF  7
#define GDT_ENTRIES 8

#ifdef CONFIG_PAE_PAGING
#define PDPTE_SIZE_BITS 3
#define PDPT_BITS    2
#define PDE_SIZE_BITS  3
#define PD_BITS      9
#define PTE_SIZE_BITS 3
#define PT_BITS      9
#define X86_GLOBAL_VSPACE_ROOT ia32KSGlobalPDPT
typedef pdpte_t vspace_root_t;
#else
#define PDPTE_SIZE_BITS 0
#define PDPT_BITS 0
#define PDE_SIZE_BITS  2
#define PD_BITS      10
#define PTE_SIZE_BITS 2
#define PT_BITS      10
#define X86_GLOBAL_VSPACE_ROOT ia32KSGlobalPD
typedef pde_t vspace_root_t;
#endif

#define PDPTE_PTR(r)   ((pdpte_t *)(r))
#define PDPTE_PTR_PTR(r) ((pdpte_t**)(r))
#define PDPTE_REF(p)   ((word_t)(p))

compile_assert(pdpt_size_bits_sane, PDPT_BITS + PDPTE_SIZE_BITS == seL4_PDPTBits)
#define PDPT_PTR(r)    ((pdpte_t*)(r))
#define PDPT_PREF(p)   ((word_t)(p))

#define PDE_PTR(r)     ((pde_t *)(r))
#define PDE_PTR_PTR(r) ((pde_t **)(r))
#define PDE_REF(p)     ((word_t)(p))

compile_assert(pd_size_sane, PD_BITS + PDE_SIZE_BITS == seL4_PageDirBits)
#define PD_PTR(r)    ((pde_t *)(r))
#define PD_REF(p)    ((word_t)(p))

#define PTE_PTR(r)    ((pte_t *)(r))
#define PTE_REF(p)    ((word_t)(p))

compile_assert(pt_size_sane, PT_BITS + PTE_SIZE_BITS == seL4_PageTableBits)
#define PT_PTR(r)    ((pte_t *)(r))
#define PT_REF(p)    ((word_t)(p))

compile_assert(gdt_idt_ptr_packed,
               sizeof(gdt_idt_ptr_t) == sizeof(uint16_t) * 3)

#define WORD_SIZE_BITS 2

enum asidSizeConstants {
    asidHighBits = 2,
    asidLowBits = 10
};

struct asid_pool {
    void* array[BIT(asidLowBits)];
};

typedef struct asid_pool asid_pool_t;

#define ASID_POOL_BITS      asidLowBits
compile_assert(asid_pool_bits_sane, ASID_POOL_BITS + WORD_SIZE_BITS == seL4_ASIDPoolBits)
#define ASID_POOL_PTR(r)    ((asid_pool_t*)r)
#define ASID_POOL_REF(p)    ((word_t)p)
#define ASID_BITS           (asidHighBits + asidLowBits)
#define nASIDPools          BIT(asidHighBits)
#define ASID_LOW(a)         (a & MASK(asidLowBits))
#define ASID_HIGH(a)        ((a >> asidLowBits) & MASK(asidHighBits))

static inline asid_t CONST
cap_frame_cap_get_capFMappedASID(cap_t cap)
{
    return
        (cap_frame_cap_get_capFMappedASIDHigh(cap) << asidLowBits) +
        cap_frame_cap_get_capFMappedASIDLow(cap);
}

static inline cap_t CONST
cap_frame_cap_set_capFMappedASID(cap_t cap, word_t asid)
{
    cap = cap_frame_cap_set_capFMappedASIDLow(cap, ASID_LOW(asid));
    return cap_frame_cap_set_capFMappedASIDHigh(cap, ASID_HIGH(asid));
}

static inline asid_t PURE
cap_frame_cap_ptr_get_capFMappedASID(cap_t* cap)
{
    return cap_frame_cap_get_capFMappedASID(*cap);
}

static inline void
cap_frame_cap_ptr_set_capFMappedASID(cap_t* cap, asid_t asid)
{
    *cap = cap_frame_cap_set_capFMappedASID(*cap, asid);
}

static inline asid_t PURE
cap_get_capMappedASID(cap_t cap)
{
    cap_tag_t ctag;

    ctag = cap_get_capType(cap);

    switch (ctag) {
    case cap_pdpt_cap:
        return cap_pdpt_cap_get_capPDPTMappedASID(cap);

    case cap_page_directory_cap:
        return cap_page_directory_cap_get_capPDMappedASID(cap);

    default:
        fail("Invalid arch cap type");
    }
}

static inline word_t CONST
cap_get_archCapSizeBits(cap_t cap)
{
    cap_tag_t ctag;

    ctag = cap_get_capType(cap);

    switch (ctag) {
    case cap_frame_cap:
        return pageBitsForSize(cap_frame_cap_get_capFSize(cap));

    case cap_page_table_cap:
        return seL4_PageTableBits;

    case cap_page_directory_cap:
        return seL4_PageDirBits;

    case cap_pdpt_cap:
        return seL4_PDPTBits;

    case cap_io_port_cap:
        return 0;
    case cap_io_space_cap:
        return 0;

    case cap_io_page_table_cap:
        return VTD_PT_SIZE_BITS;
    case cap_asid_control_cap:
        return 0;

    case cap_asid_pool_cap:
        return seL4_ASIDPoolBits;

    default:
        fail("Invalid arch cap type");
    }
}

static inline void * CONST
cap_get_archCapPtr(cap_t cap)
{
    cap_tag_t ctag;

    ctag = cap_get_capType(cap);

    switch (ctag) {

    case cap_frame_cap:
        return (void *)(cap_frame_cap_get_capFBasePtr(cap));

    case cap_page_table_cap:
        return PD_PTR(cap_page_table_cap_get_capPTBasePtr(cap));

    case cap_page_directory_cap:
        return PT_PTR(cap_page_directory_cap_get_capPDBasePtr(cap));

    case cap_pdpt_cap:
        return PDPT_PTR(cap_pdpt_cap_get_capPDPTBasePtr(cap));

    case cap_io_port_cap:
        return NULL;

    case cap_io_space_cap:
        return NULL;

    case cap_io_page_table_cap:
        return (void *)(cap_io_page_table_cap_get_capIOPTBasePtr(cap));

    case cap_asid_control_cap:
        return NULL;

    case cap_asid_pool_cap:
        return ASID_POOL_PTR(cap_asid_pool_cap_get_capASIDPool(cap));

    default:
        fail("Invalid arch cap type");
    }
}

static inline pte_t
x86_make_device_pte(paddr_t phys)
{
    return pte_new(
               phys,   /* page_base_address    */
               0,      /* avl                  */
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
               0,      /* page_base_address    */
               0,      /* avl                  */
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
               paddr,                                   /* page_base_address    */
               vm_attributes_get_x86PATBit(attr),       /* pat                  */
               0,                                       /* avl_cte_depth        */
               1,                                       /* global               */
               0,                                       /* dirty                */
               0,                                       /* accessed             */
               vm_attributes_get_x86PCDBit(attr),       /* cache_disabled       */
               vm_attributes_get_x86PWTBit(attr),       /* write_through        */
               0,                                       /* super_user           */
               1,                                       /* read_write           */
               1                                        /* present              */
           );
}

#endif
