/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#pragma once

#include <config.h>
#include <assert.h>
#include <util.h>
#include <api/types.h>
#include <sel4/macros.h>
#include <arch/types.h>
#include <arch/object/structures_gen.h>
#include <arch/machine/hardware.h>
#include <arch/machine/registerset.h>

typedef struct arch_tcb {
    user_context_t tcbContext;
#ifdef CONFIG_ARM_HYPERVISOR_SUPPORT
    struct vcpu *tcbVCPU;
#endif
} arch_tcb_t;

enum vm_rights {
    VMKernelOnly = 0,
    VMReadWrite = 1,
    VMReadOnly = 3
};
typedef word_t vm_rights_t;

#define PTE_SIZE_BITS       seL4_PageTableEntryBits
#define PT_INDEX_BITS       seL4_PageTableIndexBits

#define PT_INDEX_OFFSET     (seL4_PageBits)

#define VCPU_SIZE_BITS      seL4_VCPUBits

#ifdef AARCH64_VSPACE_S2_START_L1
/* For hyp with 40 bit PA, EL1 and EL0 use a 3 level translation and skips the PGD */
typedef pte_t vspace_root_t;
#else
/* Otherwise we use a 4-level translation */
typedef pte_t vspace_root_t;
#endif

#define VSPACE_PTR(r)       ((vspace_root_t *)(r))

#define PTE_PTR(r)          ((pte_t *)(r))
#define PTE_PTR_PTR(r)      ((pte_t **)(r))
#define PTE_REF(p)          ((word_t)(p))

#define PT_PTR(r)           ((pte_t *)(r))
#define PT_REF(p)           ((word_t)(p))

/* Generate a vcpu_t pointer from a vcpu block reference */
#define VCPU_PTR(r)       ((struct vcpu *)(r))
#define VCPU_REF(p)       ((word_t)(p))

struct asid_pool {
    asid_map_t array[BIT(asidLowBits)];
};
typedef struct asid_pool asid_pool_t;

/* Generic fastpath.c code expects pde_t for stored_hw_asid
 * that's a workaround in the time being.
 */
typedef pte_t pde_t;


#define ASID_POOL_PTR(r)    ((asid_pool_t*)r)
#define ASID_POOL_REF(p)    ((word_t)p)


#define ASID_POOL_INDEX_BITS seL4_ASIDPoolIndexBits
#define ASID_BITS (asidHighBits+asidLowBits)
#define nASIDs     BIT(ASID_BITS)
#define nASIDPools BIT(asidHighBits)

#define ASID_LOW(a) (a & MASK(asidLowBits))
#define ASID_HIGH(a) ((a >> asidLowBits) & MASK(asidHighBits))

static inline word_t CONST cap_get_archCapSizeBits(cap_t cap)
{
    cap_tag_t ctag;

    ctag = cap_get_capType(cap);

    switch (ctag) {
    case cap_frame_cap:
        return pageBitsForSize(cap_frame_cap_get_capFSize(cap));

    case cap_page_table_cap:
        return seL4_PageTableBits;

    case cap_vspace_cap:
        return seL4_VSpaceBits;

    case cap_asid_pool_cap:
        return seL4_ASIDPoolBits;

    case cap_asid_control_cap:
        return 0;

#ifdef CONFIG_ARM_HYPERVISOR_SUPPORT
    case cap_vcpu_cap:
        return seL4_VCPUBits;
#endif
#if CONFIG_MAX_NUM_NODES == 1
    case cap_sgi_signal_cap:
        return 0;
#endif

    default:
        /* Unreachable, but GCC can't figure that out */
        return 0;
    }
}

static inline bool_t CONST cap_get_archCapIsPhysical(cap_t cap)
{
    cap_tag_t ctag;

    ctag = cap_get_capType(cap);

    switch (ctag) {

    case cap_frame_cap:
        return true;

    case cap_page_table_cap:
        return true;

    case cap_vspace_cap:
        return true;

    case cap_asid_pool_cap:
        return true;

    case cap_asid_control_cap:
        return false;

#ifdef CONFIG_ARM_HYPERVISOR_SUPPORT
    case cap_vcpu_cap:
        return true;
#endif
#if CONFIG_MAX_NUM_NODES == 1
    case cap_sgi_signal_cap:
        return false;
#endif

    default:
        /* Unreachable, but GCC can't figure that out */
        return false;
    }
}

static inline void *CONST cap_get_archCapPtr(cap_t cap)
{
    cap_tag_t ctag;

    ctag = cap_get_capType(cap);

    switch (ctag) {
    case cap_frame_cap:
        return (void *)(cap_frame_cap_get_capFBasePtr(cap));

    case cap_page_table_cap:
        return PT_PTR(cap_page_table_cap_get_capPTBasePtr(cap));

    case cap_vspace_cap:
        return VSPACE_PTR(cap_vspace_cap_get_capVSBasePtr(cap));

    case cap_asid_control_cap:
        return NULL;

    case cap_asid_pool_cap:
        return ASID_POOL_PTR(cap_asid_pool_cap_get_capASIDPool(cap));

#ifdef CONFIG_ARM_HYPERVISOR_SUPPORT
    case cap_vcpu_cap:
        return VCPU_PTR(cap_vcpu_cap_get_capVCPUPtr(cap));
#endif
#if CONFIG_MAX_NUM_NODES == 1
    case cap_sgi_signal_cap:
        return NULL;
#endif

    default:
        /* Unreachable, but GCC can't figure that out */
        return NULL;
    }
}

static inline bool_t pte_pte_page_ptr_get_present(pte_t *pt)
{
    return (pte_ptr_get_pte_type(pt) == pte_pte_page);
}

static inline bool_t pte_pte_table_ptr_get_present(pte_t *pt)
{
    return (pte_ptr_get_pte_type(pt) == pte_pte_table);
}

static inline bool_t pte_4k_page_ptr_get_present(pte_t *pt)
{
    return (pte_ptr_get_pte_type(pt) == pte_pte_4k_page);
}

static inline bool_t pte_ptr_get_valid(pte_t *pt)
{
    return (pte_ptr_get_pte_type(pt) != pte_pte_invalid);
}

static inline bool_t pte_is_page_type(pte_t pte)
{
    return pte_get_pte_type(pte) == pte_pte_4k_page ||
           pte_get_pte_type(pte) == pte_pte_page;
}

/** Return base address for both of pte_4k_page and pte_page */
static inline uint64_t pte_get_page_base_address(pte_t pte)
{
    assert(pte_is_page_type(pte));
    return pte.words[0] & 0xfffffffff000ull;
}

/** Return base address for both of pte_4k_page and pte_page */
static inline uint64_t pte_page_ptr_get_page_base_address(pte_t *pt)
{
    return pte_get_page_base_address(*pt);
}
