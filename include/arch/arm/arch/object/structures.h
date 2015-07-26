/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(GD_GPL)
 */

#ifndef __ARCH_OBJECT_STRUCTURES_H
#define __ARCH_OBJECT_STRUCTURES_H

#include <assert.h>
#include <util.h>
#include <api/types.h>
#include <arch/types.h>
#include <arch/object/structures_gen.h>
#include <arch/machine/hardware.h>
#include <arch/machine/registerset.h>

/* Object sizes */

#define EP_SIZE_BITS 4
#define AEP_SIZE_BITS 4
#define CTE_SIZE_BITS 4
#define TCB_BLOCK_SIZE_BITS (TCB_SIZE_BITS+1)
enum tcb_arch_cnode_index {
    tcbArchCNodeEntries = tcbCNodeEntries
};

typedef struct arch_tcb {
    /* saved user-level context of thread (72 bytes) */
    user_context_t tcbContext;
} arch_tcb_t;

enum vm_rights {
    VMNoAccess = 0,
    VMKernelOnly = 1,
    VMReadOnly = 2,
    VMReadWrite = 3
};
typedef uint32_t vm_rights_t;

#define PDE_SIZE_BITS 2
#define PDE_PTR(r) ((pde_t *)(r))
#define PDE_REF(p) ((unsigned int)(p))

#define PDE_PTR_PTR(r) ((pde_t **)r)

#define PD_BITS 12
#define PD_SIZE_BITS (PD_BITS+PDE_SIZE_BITS)
#define PD_PTR(r) ((pde_t *)(r))
#define PD_REF(p) ((unsigned int)(p))

/* Page directory entries (PDEs) */
enum pde_type {
    PDEInvalid = 0,
    PDECoarse  = 1,
    PDEMapping = 2
};
typedef uint32_t pde_type_t;

#define PTE_SIZE_BITS 2
#define PTE_PTR(r) ((pte_t *)(r))
#define PTE_REF(p) ((unsigned int)(p))

#define PT_BITS 8
#define PT_SIZE_BITS (PT_BITS+PTE_SIZE_BITS)
#define PT_PTR(r) ((pte_t *)(r))
#define PT_REF(p) ((unsigned int)(p))

#define WORD_SIZE_BITS 2

struct user_data {
    word_t words[BIT(ARMSmallPageBits) / sizeof(word_t)];
};

typedef struct user_data user_data_t;

#define HW_ASID_SIZE_BITS 1

#define FMAPPED_OBJECT_LOW(a) ( ((uint32_t)(a) >> 10) & MASK(2))
#define FMAPPED_OBJECT_HIGH(a) ((uint32_t)(a) >> 12)

static inline uint32_t CONST
cap_frame_cap_get_capFMappedObject(cap_t cap)
{
    return (cap_frame_cap_get_capFMappedObjectHigh(cap) << 12) + (cap_frame_cap_get_capFMappedObjectLow(cap) << 10);
}

static inline cap_t CONST
cap_frame_cap_set_capFMappedObject(cap_t cap, uint32_t val)
{
    cap = cap_frame_cap_set_capFMappedObjectHigh(cap, FMAPPED_OBJECT_HIGH(val));
    return cap_frame_cap_set_capFMappedObjectLow(cap, FMAPPED_OBJECT_LOW(val));
}

static inline void
cap_frame_cap_ptr_set_capFMappedObject(cap_t *cap, uint32_t val)
{
    cap_frame_cap_ptr_set_capFMappedObjectHigh(cap, FMAPPED_OBJECT_HIGH(val));
    cap_frame_cap_ptr_set_capFMappedObjectLow(cap, FMAPPED_OBJECT_LOW(val));
}

static inline unsigned int CONST
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

        return 0;

    default:
        assert(!"Unknown cap type");
        /* Unreachable, but GCC can't figure that out */
        return 0;
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
        return PT_PTR(cap_page_table_cap_get_capPTBasePtr(cap));

    case cap_page_directory_cap:
        return PD_PTR(cap_page_directory_cap_get_capPDBasePtr(cap));

        return NULL;

    default:
        assert(!"Unknown cap type");
        /* Unreachable, but GCC can't figure that out */
        return NULL;
    }
}

/* We need to supply different type getters for the bitfield generated PTE type
 * because there is an implicit third type that PTEs can be. If the type bit is
 * set but the reserved bit is not set, the type of the PTE is invalid, not a
 * large PTE.
 */
enum { pte_pte_invalid = 2 };

static inline uint32_t __attribute__((__const__))
pte_get_pteType(pte_t pte)
{
    if (pte_get_pteSize(pte) == pte_pte_small) {
        return pte_pte_small;
    } else if (pte_pte_large_get_reserved(pte) == 1) {
        return pte_pte_large;
    } else {
        return pte_pte_invalid;
    }
}

static inline uint32_t __attribute__((__pure__))
pte_ptr_get_pteType(pte_t *pte_ptr)
{
    if (pte_ptr_get_pteSize(pte_ptr) == pte_pte_small) {
        return pte_pte_small;
    } else if (pte_pte_large_ptr_get_reserved(pte_ptr) == 1) {
        return pte_pte_large;
    } else {
        return pte_pte_invalid;
    }
}

#endif
