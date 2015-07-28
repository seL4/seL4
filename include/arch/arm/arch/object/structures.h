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

/* TCB CNode: size = 256 bytes */
/* typedef cte_t[16] tcb_cnode; */

/* TCB: size = 136 bytes, alignment = 256 bytes */
struct tcb {
    /* Saved user-level context of thread, 72 bytes */
    user_context_t tcbContext;

    /* Thread state, 12 bytes */
    thread_state_t tcbState;

    /* Current fault, 8 bytes */
    fault_t tcbFault;

    /* Current lookup failure, 8 bytes */
    lookup_fault_t tcbLookupFailure;

    /* Domain, 1 byte, padded to 4 bytes */
    dom_t tcbDomain;

    /* Priority, 1 byte, padded to 4 bytes */
    prio_t tcbPriority;

    /* Timeslice remaining, 4 bytes */
    uint32_t tcbTimeSlice;

    /* Capability pointer to thread fault handler, 4 bytes */
    cptr_t tcbFaultHandler;

    /* Physical address of thread IPC buffer, 4 bytes */
    word_t tcbIPCBuffer;

    /* Previous and next pointers for endpoint & scheduler queues, 16 bytes */
    struct tcb *tcbSchedNext, *tcbSchedPrev, *tcbEPNext, *tcbEPPrev;

#ifdef ARM_HYP
    /* Pointer to associated VCPU. NULL if not associated.
     * tcb->vcpu->tcb == tcb. */
    struct vcpu* vcpu;
#endif
#ifdef DEBUG
    /* Use any remaining space for a thread name */
    char tcbName[];
#endif
};
typedef struct tcb tcb_t;

/* ARM-specific object types */

enum vm_rights {
    VMNoAccess = 0,
    VMKernelOnly = 1,
    VMReadOnly = 2,
    VMReadWrite = 3
};
typedef uint32_t vm_rights_t;

#ifdef ARM_HYP
#define PGDE_SIZE_BITS 3
#define PDE_SIZE_BITS  3
#define PTE_SIZE_BITS  3
#define PGD_BITS 2
#define PD_BITS 11
#define PT_BITS 9
#define VCPU_SIZE_BITS 12
/* Generate a vcpu_t pointer from a vcpu block reference */
#define VCPU_PTR(r)       ((struct vcpu *)(r))
#define VCPU_REF(p)       ((unsigned int)(p))

#else /* ARM_HYP */
#define PDE_SIZE_BITS  2
#define PTE_SIZE_BITS  2
#define PD_BITS 12
#define PT_BITS 8
#endif /* ARM_HYP */


#define PDE_PTR(r) ((pde_t *)(r))
#define PDE_REF(p) ((unsigned int)p)

#define PDE_PTR_PTR(r) ((pde_t **)r)

#define PD_SIZE_BITS (PD_BITS+PDE_SIZE_BITS)
#define PD_PTR(r) ((pde_t *)(r))
#define PD_REF(p) ((unsigned int)p)

/* Page directory entries (PDEs) */
enum pde_type {
    PDEInvalid = 0,
    PDECoarse  = 1,
    PDEMapping = 2
};
typedef uint32_t pde_type_t;

#define PTE_PTR(r) ((pte_t *)r)
#define PTE_REF(p) ((unsigned int)p)

#define PT_SIZE_BITS (PT_BITS+PTE_SIZE_BITS)
#define PT_PTR(r) ((pte_t *)r)
#define PT_REF(p) ((unsigned int)p)


/* LPAE */
#define PGD_SIZE_BITS (PGD_BITS+PGDE_SIZE_BITS)
#define LPAE_PGDE_PTR(r) ((lpae_pde_t *)(r))
#define LPAE_PGDE_REF(p) ((unsigned int)p)
#define LPAE_PGDE_PTR_PTR(r) ((lpae_pde_t **)r)
#define LPAE_PGD_PTR(r) ((lpae_pde_t *)(r))
#define LPAE_PGD_REF(p) ((unsigned int)p)

#define LPAE_PTE_PTR(r) ((lpae_pte_t *)r)
#define LPAE_PTE_REF(p) ((unsigned int)p)

#define LPAE_PT_PTR(r) ((lpae_pte_t *)r)
#define LPAE_PT_REF(p) ((unsigned int)p)

#define WORD_SIZE_BITS 2

struct user_data {
    word_t words[BIT(ARMSmallPageBits) / sizeof(word_t)];
};

typedef struct user_data user_data_t;

enum asidSizeConstants {
    asidHighBits = 8,
    asidLowBits = 10
};

struct asid_pool {
    pde_t* array[BIT(asidLowBits)];
};

typedef struct asid_pool asid_pool_t;

#define ASID_POOL_PTR(r) ((asid_pool_t *)r)
#define ASID_POOL_REF(p) ((unsigned int)p)

#define HW_ASID_SIZE_BITS 1

#define ASID_POOL_BITS asidLowBits
#define ASID_POOL_SIZE_BITS (ASID_POOL_BITS+WORD_SIZE_BITS)
#define ASID_BITS (asidHighBits+asidLowBits)

#define nASIDPools BIT(asidHighBits)

#define ASID_LOW(a) (a & MASK(asidLowBits))
#define ASID_HIGH(a) ((a >> asidLowBits) & MASK(asidHighBits))

static inline cap_t CONST
cap_small_frame_cap_set_capFMappedASID(cap_t cap, word_t asid)
{
    cap = cap_small_frame_cap_set_capFMappedASIDLow(cap,
                                                    asid & MASK(asidLowBits));
    return cap_small_frame_cap_set_capFMappedASIDHigh(cap,
                                                      (asid >> asidLowBits) & MASK(asidHighBits));
}

static inline word_t CONST
cap_small_frame_cap_get_capFMappedASID(cap_t cap)
{
    return (cap_small_frame_cap_get_capFMappedASIDHigh(cap) << asidLowBits) +
           cap_small_frame_cap_get_capFMappedASIDLow(cap);
}

static inline cap_t CONST
cap_frame_cap_set_capFMappedASID(cap_t cap, word_t asid)
{
    cap = cap_frame_cap_set_capFMappedASIDLow(cap,
                                              asid & MASK(asidLowBits));
    return cap_frame_cap_set_capFMappedASIDHigh(cap,
                                                (asid >> asidLowBits) & MASK(asidHighBits));
}

static inline word_t CONST
cap_frame_cap_get_capFMappedASID(cap_t cap)
{
    return (cap_frame_cap_get_capFMappedASIDHigh(cap) << asidLowBits) +
           cap_frame_cap_get_capFMappedASIDLow(cap);
}

static inline word_t CONST
generic_frame_cap_get_capFMappedASID(cap_t cap)
{
    cap_tag_t ctag;

    ctag = cap_get_capType(cap);

    assert(ctag == cap_small_frame_cap ||
           ctag == cap_frame_cap);

    if (ctag == cap_small_frame_cap) {
        return cap_small_frame_cap_get_capFMappedASID(cap);
    } else {
        return cap_frame_cap_get_capFMappedASID(cap);
    }
}

static inline cap_t CONST
generic_frame_cap_set_capFMappedAddress(cap_t cap, word_t asid, word_t addr)
{
    cap_tag_t ctag;

    ctag = cap_get_capType(cap);
    assert(ctag == cap_small_frame_cap ||
           ctag == cap_frame_cap);

    if (ctag == cap_small_frame_cap) {
        cap = cap_small_frame_cap_set_capFMappedASID(cap, asid);
        cap = cap_small_frame_cap_set_capFMappedAddress(cap, addr);
        return cap;
    } else {
        cap = cap_frame_cap_set_capFMappedASID(cap, asid);
        cap = cap_frame_cap_set_capFMappedAddress(cap, addr);
        return cap;
    }
}

static inline void
generic_frame_cap_ptr_set_capFMappedAddress(cap_t *cap_ptr, word_t asid,
                                            word_t addr)
{
    *cap_ptr = generic_frame_cap_set_capFMappedAddress(*cap_ptr, asid, addr);
}

static inline vm_rights_t CONST
generic_frame_cap_get_capFVMRights(cap_t cap)
{
    cap_tag_t ctag;

    ctag = cap_get_capType(cap);
    assert(ctag == cap_small_frame_cap ||
           ctag == cap_frame_cap);

    switch (ctag) {
    case cap_small_frame_cap:
        return cap_small_frame_cap_get_capFVMRights(cap);

    case cap_frame_cap:
        return cap_frame_cap_get_capFVMRights(cap);

    default:
        return VMNoAccess;
    }
}

static inline word_t CONST
generic_frame_cap_get_capFBasePtr(cap_t cap)
{
    cap_tag_t ctag;

    ctag = cap_get_capType(cap);
    assert(ctag == cap_small_frame_cap ||
           ctag == cap_frame_cap);

    switch (ctag) {
    case cap_small_frame_cap:
        return cap_small_frame_cap_get_capFBasePtr(cap);

    case cap_frame_cap:
        return cap_frame_cap_get_capFBasePtr(cap);

    default:
        return 0;
    }
}

static inline word_t CONST
generic_frame_cap_get_capFSize(cap_t cap)
{
    cap_tag_t ctag;

    ctag = cap_get_capType(cap);
    assert(ctag == cap_small_frame_cap ||
           ctag == cap_frame_cap);

    switch (ctag) {
    case cap_small_frame_cap:
        return ARMSmallPage;

    case cap_frame_cap:
        return cap_frame_cap_get_capFSize(cap);

    default:
        return 0;
    }
}

static inline word_t CONST
generic_frame_cap_get_capFIsMapped(cap_t cap)
{
    return generic_frame_cap_get_capFMappedASID(cap) != 0;
}

static inline word_t CONST
generic_frame_cap_get_capFMappedAddress(cap_t cap)
{
    cap_tag_t ctag;

    ctag = cap_get_capType(cap);
    assert(ctag == cap_small_frame_cap ||
           ctag == cap_frame_cap);

    if (ctag == cap_small_frame_cap) {
        return cap_small_frame_cap_get_capFMappedAddress(cap);
    } else {
        return cap_frame_cap_get_capFMappedAddress(cap);
    }
}

static inline unsigned int CONST
cap_get_capSizeBits(cap_t cap)
{
    cap_tag_t ctag;

    ctag = cap_get_capType(cap);

    switch (ctag) {
    case cap_untyped_cap:
        return cap_untyped_cap_get_capBlockSize(cap);

    case cap_endpoint_cap:
        return EP_SIZE_BITS;

    case cap_async_endpoint_cap:
        return AEP_SIZE_BITS;

    case cap_cnode_cap:
        return cap_cnode_cap_get_capCNodeRadix(cap) + CTE_SIZE_BITS;

    case cap_thread_cap:
        return TCB_BLOCK_SIZE_BITS;

    case cap_small_frame_cap:
    case cap_frame_cap:
        return pageBitsForSize(generic_frame_cap_get_capFSize(cap));

    case cap_page_table_cap:
        return PT_SIZE_BITS;

    case cap_page_directory_cap:
        return PD_SIZE_BITS;

    case cap_asid_pool_cap:
        return ASID_POOL_SIZE_BITS;

    case cap_zombie_cap: {
        uint32_t type = cap_zombie_cap_get_capZombieType(cap);
        if (type == ZombieType_ZombieTCB) {
            return TCB_BLOCK_SIZE_BITS;
        }
        return ZombieType_ZombieCNode(type) + CTE_SIZE_BITS;
    }

    case cap_null_cap:
    case cap_domain_cap:
    case cap_reply_cap:
    case cap_irq_control_cap:
    case cap_irq_handler_cap:
    case cap_asid_control_cap:
        return 0;

#ifdef ARM_HYP
    case cap_vcpu_cap:
        return VCPU_SIZE_BITS;
#endif

    default:
        /* Unreachable, but GCC can't figure that out */
        return 0;
    }
}

static inline void * CONST
cap_get_capPtr(cap_t cap)
{
    cap_tag_t ctag;

    ctag = cap_get_capType(cap);

    switch (ctag) {
    case cap_untyped_cap:
        return WORD_PTR(cap_untyped_cap_get_capPtr(cap));

    case cap_endpoint_cap:
        return EP_PTR(cap_endpoint_cap_get_capEPPtr(cap));

    case cap_async_endpoint_cap:
        return AEP_PTR(cap_async_endpoint_cap_get_capAEPPtr(cap));

    case cap_cnode_cap:
        return CTE_PTR(cap_cnode_cap_get_capCNodePtr(cap));

    case cap_thread_cap:
        return TCB_PTR_CTE_PTR(cap_thread_cap_get_capTCBPtr(cap), 0);

    case cap_small_frame_cap:
    case cap_frame_cap:
        return (void *)(generic_frame_cap_get_capFBasePtr(cap));

    case cap_page_table_cap:
        return PT_PTR(cap_page_table_cap_get_capPTBasePtr(cap));

    case cap_page_directory_cap:
        return PD_PTR(cap_page_directory_cap_get_capPDBasePtr(cap));

    case cap_asid_pool_cap:
        return ASID_POOL_PTR(cap_asid_pool_cap_get_capASIDPool(cap));

    case cap_zombie_cap:
        return CTE_PTR(cap_zombie_cap_get_capZombiePtr(cap));

    case cap_null_cap:
    case cap_domain_cap:
    case cap_reply_cap:
    case cap_irq_control_cap:
    case cap_irq_handler_cap:
    case cap_asid_control_cap:
        return NULL;

#ifdef ARM_HYP
    case cap_vcpu_cap:
        return VCPU_PTR(cap_vcpu_cap_get_capVCPUPtr(cap));
#endif

    default:
        /* Unreachable, but GCC can't figure that out */
        return NULL;
    }
}

static inline word_t CONST
isArchCap(cap_t cap)
{
    return (cap_get_capType(cap) % 2);
}

#ifndef ARM_HYP
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
#endif /* ARM_HYP */

#endif
