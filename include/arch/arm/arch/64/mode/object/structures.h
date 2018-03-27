/*
 * Copyright 2017, Data61
 * Commonwealth Scientific and Industrial Research Organisation (CSIRO)
 * ABN 41 687 119 230.
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(DATA61_GPL)
 */

#ifndef __ARCH_OBJECT_STRUCTURES_64_H
#define __ARCH_OBJECT_STRUCTURES_64_H

#include <config.h>
#include <assert.h>
#include <util.h>
#include <api/types.h>
#include <api/macros.h>
#include <arch/types.h>
#include <arch/object/structures_gen.h>
#include <arch/machine/hardware.h>
#include <arch/machine/registerset.h>

typedef struct arch_tcb {
    user_context_t tcbContext;
#ifdef CONFIG_ARM_HYPERVISOR_SUPPORT
    struct vcpu* tcbVCPU;
#endif
} arch_tcb_t;

enum vm_rights {
    VMKernelOnly = 0,
    VMReadWrite = 1,
    VMKernelReadOnly = 2,
    VMReadOnly = 3
};
typedef word_t vm_rights_t;

#define PGDE_SIZE_BITS      seL4_PGDEntryBits
#define PGD_INDEX_BITS      seL4_PGDIndexBits
#define PUDE_SIZE_BITS      seL4_PUDEntryBits
#define PUD_INDEX_BITS      seL4_PUDIndexBits
#define PDE_SIZE_BITS       seL4_PageDirEntryBits
#define PD_INDEX_BITS       seL4_PageDirIndexBits
#define PTE_SIZE_BITS       seL4_PageTableEntryBits
#define PT_INDEX_BITS       seL4_PageTableIndexBits

#define PT_INDEX_OFFSET     (seL4_PageBits)
#define PD_INDEX_OFFSET     (PT_INDEX_OFFSET + PT_INDEX_BITS)
#define PUD_INDEX_OFFSET    (PD_INDEX_OFFSET + PD_INDEX_BITS)
#define PGD_INDEX_OFFSET    (PUD_INDEX_OFFSET + PUD_INDEX_BITS)

#define VCPU_SIZE_BITS      seL4_VCPUBits

typedef pgde_t vspace_root_t;

#define GET_PGD_INDEX(x)    (((x) >> (PGD_INDEX_OFFSET)) & MASK(PGD_INDEX_BITS))
#define GET_PUD_INDEX(x)    (((x) >> (PUD_INDEX_OFFSET)) & MASK(PUD_INDEX_BITS))
#define GET_PD_INDEX(x)     (((x) >> (PD_INDEX_OFFSET)) & MASK(PD_INDEX_BITS))
#define GET_PT_INDEX(x)     (((x) >> (PT_INDEX_OFFSET)) & MASK(PT_INDEX_BITS))

#define PGDE_PTR(r)         ((pgde_t *)(r))
#define PGDE_PTR_PTR(r)     ((pgde_t **)(r))
#define PGDE_REF(p)         ((word_t)(p))

#define PGD_PTR(r)          ((pgde_t *)(r))
#define PGD_REF(p)          ((word_t)(r))

#define PUDE_PTR(r)         ((pude_t *)(r))
#define PUDE_PTR_PTR(r)     ((pude_t **)(r))
#define PUDE_REF(p)         ((word_t)(p))

#define PUD_PTR(r)          ((pude_t *)(r))
#define PUD_PREF(p)         ((word_t)(p))

#define PDE_PTR(r)          ((pde_t *)(r))
#define PDE_PTR_PTR(r)      ((pde_t **)(r))
#define PDE_REF(p)          ((word_t)(p))

#define PD_PTR(r)           ((pde_t *)(r))
#define PD_REF(p)           ((word_t)(p))

#define PTE_PTR(r)          ((pte_t *)(r))
#define PTE_REF(p)          ((word_t)(p))

#define PT_PTR(r)           ((pte_t *)(r))
#define PT_REF(p)           ((word_t)(p))

/* Generate a vcpu_t pointer from a vcpu block reference */
#define VCPU_PTR(r)       ((struct vcpu *)(r))
#define VCPU_REF(p)       ((word_t)(p))

enum asidSizeConstants {
    asidHighBits = 7,
    asidLowBits = seL4_ASIDPoolIndexBits
};

struct asid_pool {
    vspace_root_t* array[BIT(asidLowBits)];
};
typedef struct asid_pool asid_pool_t;

#define ASID_POOL_PTR(r)    ((asid_pool_t*)r)
#define ASID_POOL_REF(p)    ((word_t)p)


#define ASID_POOL_INDEX_BITS seL4_ASIDPoolIndexBits
#define ASID_BITS (asidHighBits+asidLowBits)

#define nASIDPools BIT(asidHighBits)

#define ASID_LOW(a) (a & MASK(asidLowBits))
#define ASID_HIGH(a) ((a >> asidLowBits) & MASK(asidHighBits))

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

    case cap_page_upper_directory_cap:
        return seL4_PUDBits;

    case cap_page_global_directory_cap:
        return seL4_PGDBits;

    case cap_asid_pool_cap:
        return seL4_ASIDPoolBits;

    case cap_asid_control_cap:
        return 0;

    default:
        /* Unreachable, but GCC can't figure that out */
        return 0;
    }
}

static inline bool_t CONST
cap_get_archCapIsPhysical(cap_t cap)
{
    cap_tag_t ctag;

    ctag = cap_get_capType(cap);

    switch (ctag) {

    case cap_frame_cap:
        return true;

    case cap_page_table_cap:
        return true;

    case cap_page_directory_cap:
        return true;

    case cap_page_upper_directory_cap:
        return true;

    case cap_page_global_directory_cap:
        return true;

    case cap_asid_pool_cap:
        return true;

    case cap_asid_control_cap:
        return false;

    default:
        /* Unreachable, but GCC can't figure that out */
        return false;
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

    case cap_page_upper_directory_cap:
        return PUD_PTR(cap_page_upper_directory_cap_get_capPUDBasePtr(cap));

    case cap_page_global_directory_cap:
        return PGD_PTR(cap_page_global_directory_cap_get_capPGDBasePtr(cap));

    case cap_asid_control_cap:
        return NULL;

    case cap_asid_pool_cap:
        return ASID_POOL_PTR(cap_asid_pool_cap_get_capASIDPool(cap));

    default:
        /* Unreachable, but GCC can't figure that out */
        return NULL;
    }
}

static inline bool_t
pgde_ptr_get_present(pgde_t *pgd)
{
    return (pgde_ptr_get_reserved(pgd) == 0b11);
}

static inline pgde_t
pgde_invalid_new(void)
{
    return (pgde_t) {
        {
            0
        }
    };
}

static inline bool_t
pude_pude_pd_ptr_get_present(pude_t *pud)
{
    return (pude_ptr_get_pude_type(pud) == pude_pude_pd);
}

static inline bool_t
pude_pude_1g_ptr_get_present(pude_t *pud)
{
    return (pude_ptr_get_pude_type(pud) == pude_pude_1g);
}

static inline pude_t
pude_invalid_new(void)
{
    return (pude_t) {
        {
            0
        }
    };
}

static inline bool_t
pde_pde_small_ptr_get_present(pde_t *pd)
{
    return (pde_ptr_get_pde_type(pd) == pde_pde_small);
}

static inline bool_t
pde_pde_large_ptr_get_present(pde_t *pd)
{
    return (pde_ptr_get_pde_type(pd) == pde_pde_large);
}

static inline pde_t
pde_invalid_new(void)
{
    return (pde_t) {
        {
            0
        }
    };
}

static inline bool_t
pte_ptr_get_present(pte_t *pt)
{
    return (pte_ptr_get_reserved(pt) == 0b11);
}

static inline pte_t
pte_invalid_new(void)
{
    return (pte_t) {
        {
            0
        }
    };
}

#endif /* __ARCH_OBJECT_STRUCTURES_64_H */
