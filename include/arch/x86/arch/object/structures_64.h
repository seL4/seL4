/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(GD_GPL)
 */

#ifndef __ARCH_OBJECT_STRUCTURES_64_H
#define __ARCH_OBJECT_STRUCTURES_64_H

/* Object sizes*/
#define EP_SIZE_BITS  4
#define NTFN_SIZE_BITS 5
#define CTE_SIZE_BITS 5
#define TCB_BLOCK_SIZE_BITS 11
/* update this when you modify the tcb struct */
#define EXPECTED_TCB_SIZE sizeof(tcb_t)

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
#define PML4_BITS 9
#define PDPTE_SIZE_BITS 3
#define PDPT_BITS 9
#define PDE_SIZE_BITS  3
#define PD_BITS      9
#define PTE_SIZE_BITS 3
#define PT_BITS      9

typedef pml4e_t vspace_root_t;

#define GET_PML4_INDEX(x) ( ((x) >> (PAGE_BITS + PT_BITS + PD_BITS + PDPT_BITS)) & MASK(PML4_BITS))
#define GET_PDPT_INDEX(x) ( ((x) >> (PAGE_BITS + PT_BITS + PD_BITS)) & MASK(PDPT_BITS))
#define GET_PD_INDEX(x)   ( ((x) >> (PAGE_BITS + PT_BITS)) & MASK(PD_BITS))
#define GET_PT_INDEX(x)   ( ((x) >> (PAGE_BITS)) & MASK(PT_BITS))

#define PML4E_PTR(r)     ((pml4e_t *)(r))
#define PML4E_PTR_PTR(r) ((pml4e_t **)(r))
#define PML4E_REF(p)     ((word_t)(p))

#define PML4_SIZE_BITS (PML4_BITS + PML4E_SIZE_BITS)
#define PML4_PTR(r)     ((pml4e_t *)(r))
#define PML4_REF(p)     ((word_t)(r))

#define PDPTE_PTR(r)   ((pdpte_t *)(r))
#define PDPTE_PTR_PTR(r) ((pdpte_t **)(r))
#define PDPTE_REF(p)   ((word_t)(p))

#define PDPT_SIZE_BITS (PDPT_BITS + PDPTE_SIZE_BITS)
#define PDPT_PTR(r)    ((pdpte_t *)(r))
#define PDPT_PREF(p)   ((word_t)(p))

#define PDE_PTR(r)     ((pde_t *)(r))
#define PDE_PTR_PTR(r) ((pde_t **)(r))
#define PDE_REF(p)     ((word_t)(p))

#define PD_SIZE_BITS (PD_BITS + PDE_SIZE_BITS)
#define PD_PTR(r)    ((pde_t *)(r))
#define PD_REF(p)    ((word_t)(p))

#define PTE_PTR(r)    ((pte_t *)(r))
#define PTE_REF(p)    ((word_t)(p))

#define PT_SIZE_BITS (PT_BITS + PTE_SIZE_BITS)
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
    void* array[BIT(asidLowBits)];
};

typedef struct asid_pool asid_pool_t;

#define ASID_POOL_BITS      asidLowBits
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
cap_get_archCapSizeBits(cap_t cap)
{
    cap_tag_t ctag;

    ctag = cap_get_capType(cap);

    switch (ctag) {
    case cap_frame_cap:
        return pageBitsForSize(cap_frame_cap_get_capFSize(cap));

    case cap_page_table_cap:
        return PT_SIZE_BITS;

    case cap_page_directory_cap:
        return PD_SIZE_BITS;

    case cap_pdpt_cap:
        return PDPT_SIZE_BITS;

    case cap_pml4_cap:
        return PML4_SIZE_BITS;

    case cap_io_port_cap:
        return 0;

    case cap_io_space_cap:
        return 0;

    case cap_io_page_table_cap:
        return VTD_PT_SIZE_BITS;

    case cap_asid_control_cap:
        return 0;

    case cap_asid_pool_cap:
        return ASID_POOL_SIZE_BITS;

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

    case cap_pml4_cap:
        return PML4_PTR(cap_pml4_cap_get_capPML4BasePtr(cap));

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
x86_make_device_pte(paddr_t phys) {
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
x86_make_empty_pte(void) {
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

#endif
