/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(GD_GPL)
 */

#ifndef __OBJECT_STRUCTURES_H
#define __OBJECT_STRUCTURES_H

#include <api/types.h>
#include <stdint.h>
#include <arch/object/structures_gen.h>

enum irq_state {
    IRQInactive  = 0,
    IRQNotifyAEP = 1,
    IRQTimer     = 2,
    IRQReserved  = 3,
};
typedef uint32_t irq_state_t;

typedef struct dschedule {
    dom_t domain;
    uint32_t length;
} dschedule_t;

/* Arch-independent object types */

enum endpoint_state {
    EPState_Idle = 0,
    EPState_Send = 1,
    EPState_Recv = 2
};
typedef uint32_t endpoint_state_t;

#define EP_SIZE_BITS 4
#define EP_PTR(r) ((endpoint_t *)(r))
#define EP_REF(p) ((unsigned int)(p))

enum async_endpoint_state {
    AEPState_Idle    = 0,
    AEPState_Waiting = 1,
    AEPState_Active  = 2
};
typedef uint32_t async_endpoint_state_t;

/* Declare object casts. As the sizes of objects may
 * differ by architecture, they are declared in the
 * arch structures.h
 */
#define EP_PTR(r) ((endpoint_t *)(r))
#define EP_REF(p) ((unsigned int)(p))

#define AEP_PTR(r) ((async_endpoint_t *)(r))
#define AEP_REF(p) ((unsigned int)(p))

#define CTE_PTR(r) ((cte_t *)(r))
#define CTE_REF(p) ((unsigned int)(p))

#define CNODE_MIN_BITS 1
#define CNODE_PTR(r) (CTE_PTR(r))
#define CNODE_REF(p) (CTE_REF(p)>>CNODE_MIN_BITS)

#define TCB_CNODE_RADIX     4
#define TCB_SIZE_BITS       (TCB_CNODE_RADIX + CTE_SIZE_BITS)
#define TCB_OFFSET          (1 << TCB_SIZE_BITS)

/* Generate a tcb_t or cte_t pointer from a tcb block reference */
#define TCB_PTR(r)       ((tcb_t *)(r))
#define TCB_CTE_PTR(r,i) (((cte_t *)(r))+(i))
#define TCB_REF(p)       ((unsigned int)(p))

/* Generate a cte_t pointer from a tcb_t pointer */
#define TCB_PTR_CTE_PTR(p,i) \
    (((cte_t *)((unsigned int)(p)&~MASK(TCB_BLOCK_SIZE_BITS)))+(i))

#define WORD_BITS   (8 * sizeof(word_t))
#define WORD_PTR(r) ((word_t *)(r))
#define WORD_REF(p) ((unsigned int)(p))

#define ZombieType_ZombieTCB        BIT(5)
#define ZombieType_ZombieCNode(n)   ((n) & MASK(5))

static inline cap_t CONST
Zombie_new(uint32_t number, uint32_t type, uint32_t ptr)
{
    uint32_t mask;

    if (type == ZombieType_ZombieTCB) {
        mask = MASK(TCB_CNODE_RADIX + 1);
    } else {
        mask = MASK(type + 1);
    }

    return cap_zombie_cap_new((ptr & ~mask) | (number & mask), type);
}

static inline uint32_t CONST
cap_zombie_cap_get_capZombieBits(cap_t cap)
{
    uint32_t type = cap_zombie_cap_get_capZombieType(cap);
    if (type == ZombieType_ZombieTCB) {
        return TCB_CNODE_RADIX;
    }
    return ZombieType_ZombieCNode(type); /* cnode radix */
}

static inline uint32_t CONST
cap_zombie_cap_get_capZombieNumber(cap_t cap)
{
    uint32_t radix = cap_zombie_cap_get_capZombieBits(cap);
    return cap_zombie_cap_get_capZombieID(cap) & MASK(radix + 1);
}

static inline uint32_t CONST
cap_zombie_cap_get_capZombiePtr(cap_t cap)
{
    uint32_t radix = cap_zombie_cap_get_capZombieBits(cap);
    return cap_zombie_cap_get_capZombieID(cap) & ~MASK(radix + 1);
}

static inline cap_t CONST
cap_zombie_cap_set_capZombieNumber(cap_t cap, uint32_t n)
{
    uint32_t radix = cap_zombie_cap_get_capZombieBits(cap);
    uint32_t ptr = cap_zombie_cap_get_capZombieID(cap) & ~MASK(radix + 1);
    return cap_zombie_cap_set_capZombieID(cap, ptr | (n & MASK(radix + 1)));
}

/* Capability table entry (CTE) */
struct cte {
    cap_t cap;
    mdb_node_t cteMDBNode;
};
typedef struct cte cte_t;

#define nullMDBNode mdb_node_new(0, false, false, 0)

/* Thread state */
enum _thread_state {
    ThreadState_Inactive = 0,
    ThreadState_Running,
    ThreadState_Restart,
    ThreadState_BlockedOnReceive,
    ThreadState_BlockedOnSend,
    ThreadState_BlockedOnReply,
    ThreadState_BlockedOnAsyncEvent,
    ThreadState_RunningVM,
    ThreadState_IdleThreadState
};
typedef uint32_t _thread_state_t;

/* A TCB CNode and a TCB are always allocated together, and adjacently,
 *  * such that they fill a 1024-byte aligned block. The CNode comes first. */
enum tcb_cnode_index {
    /* CSpace root, 16 bytes */
    tcbCTable = 0,

    /* VSpace root, 16 bytes */
    tcbVTable = 1,

    /* Reply cap slot, 16 bytes */
    tcbReply = 2,

    /* TCB of most recent IPC sender, 16 bytes */
    tcbCaller = 3,

    /* IPC buffer cap slot, 16 bytes */
    tcbBuffer = 4,

    tcbCNodeEntries
};
typedef uint32_t tcb_cnode_index_t;

#define CTE_DEPTH_BITS 5

#include <arch/object/structures.h>

static inline word_t CONST
wordFromVMRights(vm_rights_t vm_rights)
{
    return (word_t)vm_rights;
}

static inline vm_rights_t CONST
vmRightsFromWord(word_t w)
{
    return (vm_rights_t)w;
}

static inline vm_attributes_t CONST
vmAttributesFromWord(word_t w)
{
    vm_attributes_t attr;

    attr.words[0] = w;
    return attr;
}

/* Ensure object sizes are sane */
compile_assert(cte_size_sane, sizeof(cte_t) <= (1 << CTE_SIZE_BITS))
compile_assert(ep_size_sane, sizeof(endpoint_t) <= (1 << EP_SIZE_BITS))
compile_assert(aep_size_sane, sizeof(async_endpoint_t) <= (1 << AEP_SIZE_BITS))

static inline word_t mdb_node_get_cdtLeft(mdb_node_t mdb)
{
    word_t cte = mdb_node_get_cdtLeft_(mdb) << 2;
    return cte == 0 ? 0 : cte | 0xE0000000;
}

static inline word_t mdb_node_get_cdtRight(mdb_node_t mdb)
{
    word_t cte = mdb_node_get_cdtRight_(mdb) << 2;
    return cte == 0 ? 0 : cte | 0xE0000000;
}

static inline void mdb_node_ptr_set_cdtLeft(mdb_node_t *mdb, word_t cte)
{
    assert((cte & MASK(2)) == 0);
    mdb_node_ptr_set_cdtLeft_(mdb, (cte & 0x1FFFFFFF) >> 2);
}

static inline void mdb_node_ptr_set_cdtRight(mdb_node_t *mdb, word_t cte)
{
    assert((cte & MASK(2)) == 0);
    mdb_node_ptr_set_cdtRight_(mdb, (cte & 0x1FFFFFFF) >> 2);
}

/* TCB: size 64 bytes + sizeof(arch_tcb_t) (aligned to nearest power of 2) */
struct tcb {
    /* arch specific tcb state (including context)*/
    arch_tcb_t tcbArch;

    /* Thread state, 12 bytes */
    thread_state_t tcbState;

    /* Bound AEP 4 bytes */
    async_endpoint_t *boundAsyncEndpoint;

    /* Current fault, 8 bytes */
    fault_t tcbFault;

    /* Current lookup failure, 8 bytes */
    lookup_fault_t tcbLookupFailure;

    /* Domain, 1 byte (packed to 4) */
    uint32_t tcbDomain;

    /* Priority, 1 byte (packed to 4) */
    uint32_t tcbPriority;

    /* Timeslice remaining, 4 bytes */
    word_t tcbTimeSlice;

    /* Capability pointer to thread fault handler, 4 bytes */
    cptr_t tcbFaultHandler;

    /* userland virtual address of thread IPC buffer, 4 bytes */
    word_t tcbIPCBuffer;

    /* Previous and next pointers for endpoint & scheduler queues, 16 bytes */
    struct tcb* tcbSchedNext;
    struct tcb* tcbSchedPrev;
    struct tcb* tcbEPNext;
    struct tcb* tcbEPPrev;

#ifdef DEBUG
    /* Use any remaining space for a thread name */
    char tcbName[];
#endif
};
typedef struct tcb tcb_t;

/* Ensure TCB size is sane. */
compile_assert(tcb_size_sane,
               (1 << TCB_SIZE_BITS) + sizeof(tcb_t) <= (1 << TCB_BLOCK_SIZE_BITS))


/* helper functions */

static inline word_t CONST
isArchCap(cap_t cap)
{
    return (cap_get_capType(cap) % 2);
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

    case cap_zombie_cap: {
        uint32_t type = cap_zombie_cap_get_capZombieType(cap);
        if (type == ZombieType_ZombieTCB) {
            return TCB_BLOCK_SIZE_BITS;
        }
        return ZombieType_ZombieCNode(type) + CTE_SIZE_BITS;
    }

    case cap_null_cap:
        return 0;

    case cap_domain_cap:
        return 0;

    case cap_reply_cap:
        return 0;

    case cap_irq_control_cap:
        return 0;

    case cap_irq_handler_cap:
        return 0;

    default:
        return cap_get_archCapSizeBits(cap);
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

    case cap_zombie_cap:
        return CTE_PTR(cap_zombie_cap_get_capZombiePtr(cap));

    case cap_domain_cap:
        return NULL;

    case cap_reply_cap:
        return NULL;

    case cap_irq_control_cap:
        return NULL;

    case cap_irq_handler_cap:
        return NULL;
    default:
        return cap_get_archCapPtr(cap);

    }
}

static inline unsigned int CONST
cap_get_capBadge(cap_t cap)
{
    switch (cap_get_capType(cap)) {
    case cap_endpoint_cap:
        return cap_endpoint_cap_get_capEPBadge(cap);
    case cap_async_endpoint_cap:
        return cap_async_endpoint_cap_get_capAEPBadge(cap);
    }
    return 0;
}

#include <arch/object/capspace.h>

#endif
