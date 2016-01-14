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
    IRQSignal    = 1,
    IRQTimer     = 2,
    IRQReserved  = 3,
};
typedef word_t irq_state_t;

/* Arch-independent object types */
enum endpoint_state {
    EPState_Idle = 0,
    EPState_Send = 1,
    EPState_Recv = 2
};
typedef word_t endpoint_state_t;

enum notification_state {
    NtfnState_Idle    = 0,
    NtfnState_Waiting = 1,
    NtfnState_Active  = 2
};
typedef word_t notification_state_t;

#define EP_PTR(r) ((endpoint_t *)(r))
#define EP_REF(p) ((word_t)(p))

#define NTFN_PTR(r) ((notification_t *)(r))
#define NTFN_REF(p) ((word_t)(p))

#define CTE_PTR(r) ((cte_t *)(r))
#define CTE_REF(p) ((word_t)(p))

#define CNODE_MIN_BITS 1
#define CNODE_PTR(r) (CTE_PTR(r))
#define CNODE_REF(p) (CTE_REF(p)>>CNODE_MIN_BITS)

#define TCB_CNODE_RADIX     4
#define TCB_SIZE_BITS       (TCB_CNODE_RADIX + CTE_SIZE_BITS)
#define TCB_OFFSET          (1 << TCB_SIZE_BITS)

/* Generate a tcb_t or cte_t pointer from a tcb block reference */
#define TCB_PTR(r)       ((tcb_t *)(r))
#define TCB_CTE_PTR(r,i) (((cte_t *)(r))+(i))
#define TCB_REF(p)       ((word_t)(p))

#define SC_REF(p) ((word_t) (p))
#define SC_PTR(r) ((sched_context_t *) (r))
#define SC_SIZE_BITS 6

/* Generate a cte_t pointer from a tcb_t pointer */
#define TCB_PTR_CTE_PTR(p,i) \
    (((cte_t *)((word_t)(p)&~MASK(TCB_BLOCK_SIZE_BITS)))+(i))

#define WORD_BITS   (8 * sizeof(word_t))
#define WORD_PTR(r) ((word_t *)(r))
#define WORD_REF(p) ((word_t)(p))

#define ZombieType_ZombieTCB        BIT(5)
#define ZombieType_ZombieCNode(n)   ((n) & MASK(5))

static inline cap_t CONST
Zombie_new(word_t number, word_t type, word_t ptr)
{
    word_t mask;

    if (type == ZombieType_ZombieTCB) {
        mask = MASK(TCB_CNODE_RADIX + 1);
    } else {
        mask = MASK(type + 1);
    }

    return cap_zombie_cap_new((ptr & ~mask) | (number & mask), type);
}

static inline word_t CONST
cap_zombie_cap_get_capZombieBits(cap_t cap)
{
    word_t type = cap_zombie_cap_get_capZombieType(cap);
    if (type == ZombieType_ZombieTCB) {
        return TCB_CNODE_RADIX;
    }
    return ZombieType_ZombieCNode(type); /* cnode radix */
}

static inline word_t CONST
cap_zombie_cap_get_capZombieNumber(cap_t cap)
{
    word_t radix = cap_zombie_cap_get_capZombieBits(cap);
    return cap_zombie_cap_get_capZombieID(cap) & MASK(radix + 1);
}

static inline word_t CONST
cap_zombie_cap_get_capZombiePtr(cap_t cap)
{
    word_t radix = cap_zombie_cap_get_capZombieBits(cap);
    return cap_zombie_cap_get_capZombieID(cap) & ~MASK(radix + 1);
}

static inline cap_t CONST
cap_zombie_cap_set_capZombieNumber(cap_t cap, word_t n)
{
    word_t radix = cap_zombie_cap_get_capZombieBits(cap);
    word_t ptr = cap_zombie_cap_get_capZombieID(cap) & ~MASK(radix + 1);
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
    ThreadState_BlockedOnNotification,
    ThreadState_IdleThreadState
};
typedef word_t _thread_state_t;

/* A TCB CNode and a TCB are always allocated together, and adjacently.
 * The CNode comes first. */
enum tcb_cnode_index {
    /* CSpace root */
    tcbCTable = 0,

    /* VSpace root */
    tcbVTable = 1,

    /* Reply cap slot */
    tcbReply = 2,

    /* TCB of most recent IPC sender */
    tcbCaller = 3,

    /* IPC buffer cap slot */
    tcbBuffer = 4,

    tcbFaultHandler = 5,

    tcbTemporalFaultHandler = 6,

    tcbCNodeEntries
};
typedef word_t tcb_cnode_index_t;

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

/* TCB: size 60 bytes + sizeof(arch_tcb_t) (aligned to nearest power of 2) */
typedef struct sched_context sched_context_t;

struct tcb {
    /* arch specific tcb state (including context)*/
    arch_tcb_t tcbArch;

    /* Thread state, 12 bytes */
    thread_state_t tcbState;

    /* Notification that this TCB is bound to. If this is set, when this TCB waits on
     * any sync endpoint, it may receive a signal from a Notification object.
     * 4 bytes*/
    notification_t *tcbBoundNotification;

    /* Current fault, 8 bytes */
    fault_t tcbFault;

    /* Current lookup failure, 8 bytes */
    lookup_fault_t tcbLookupFailure;

    /* Priority, 1 byte (packed to 4) */
    prio_t tcbPriority;

    /* max priority, 1 byte (packed to 4) */
    prio_t tcbMaxPriority;

    /* sched context object for this tcb, 4 bytes */
    sched_context_t *tcbSchedContext;

    /* userland virtual address of thread IPC buffer, 4 bytes */
    word_t tcbIPCBuffer;

    /* Previous and next pointers for scheduler queues , 8 bytes */
    struct tcb* tcbSchedNext;
    struct tcb* tcbSchedPrev;
    /* Preivous and next pointers for endpoint and notification queues, 8 bytes */
    struct tcb* tcbEPNext;
    struct tcb* tcbEPPrev;

#ifdef DEBUG
    /* Use any remaining space for a thread name */
    char tcbName[];
#endif
};
typedef struct tcb tcb_t;

/* sched context - 48 bytes */
struct sched_context {
    /* budget for this tcb -- remaining is refilled from this value */
    ticks_t scBudget;

    /* period for this tcb -- controls rate at which budget is replenished */
    ticks_t scPeriod;

    /* current budget for this tcb (timeslice) -- will be refilled from budget */
    ticks_t scRemaining;

    /* timestamp when this scheduling context is next due for a budget recharge */
    ticks_t scNext;

    /* tcb that is currently running on this scheduling context */
    tcb_t *scTcb;

    /* if this scheduling context was donated over IPC, this field stores a pointer
     * to thread that holds the reply cap, if a reply cap was generated.
     * This allows the scheduling context
     * to can get back to the original caller even if a different thread replies on
     * behalf of the caller or the reply cap is deleted */
    tcb_t *scReply;

    /* notification this scheduling context is optionally bound to */
    notification_t *scNotification;

    /* data word that is sent with temporal faults that occur on this scheduling context */
    seL4_Word scData;
};

/* Ensure object sizes are sane */
compile_assert(cte_size_sane, sizeof(cte_t) <= (1 << CTE_SIZE_BITS))
compile_assert(tcb_size_expected, sizeof(tcb_t) == EXPECTED_TCB_SIZE)
compile_assert(tcb_size_sane,
               (1 << TCB_SIZE_BITS) + sizeof(tcb_t) <= (1 << TCB_BLOCK_SIZE_BITS))
compile_assert(ep_size_sane, sizeof(endpoint_t) <= (1 << EP_SIZE_BITS))
compile_assert(notification_size_sane, sizeof(notification_t) <= (1 << NTFN_SIZE_BITS))
compile_assert(sc_size_sane, sizeof(sched_context_t) <= (1 << SC_SIZE_BITS))

/* helper functions */

static inline word_t CONST
isArchCap(cap_t cap)
{
    return (cap_get_capType(cap) % 2);
}

static inline word_t CONST
cap_get_capSizeBits(cap_t cap)
{

    cap_tag_t ctag;

    ctag = cap_get_capType(cap);

    switch (ctag) {
    case cap_untyped_cap:
        return cap_untyped_cap_get_capBlockSize(cap);

    case cap_endpoint_cap:
        return EP_SIZE_BITS;

    case cap_notification_cap:
        return NTFN_SIZE_BITS;

    case cap_cnode_cap:
        return cap_cnode_cap_get_capCNodeRadix(cap) + CTE_SIZE_BITS;

    case cap_thread_cap:
        return TCB_BLOCK_SIZE_BITS;

    case cap_zombie_cap: {
        word_t type = cap_zombie_cap_get_capZombieType(cap);
        if (type == ZombieType_ZombieTCB) {
            return TCB_BLOCK_SIZE_BITS;
        }
        return ZombieType_ZombieCNode(type) + CTE_SIZE_BITS;
    }

    case cap_null_cap:
        return 0;

    case cap_reply_cap:
        return 0;

    case cap_irq_control_cap:
        return 0;

    case cap_irq_handler_cap:
        return 0;

    case cap_sched_context_cap:
        return SC_SIZE_BITS;

    case cap_sched_control_cap:
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

    case cap_notification_cap:
        return NTFN_PTR(cap_notification_cap_get_capNtfnPtr(cap));

    case cap_cnode_cap:
        return CTE_PTR(cap_cnode_cap_get_capCNodePtr(cap));

    case cap_thread_cap:
        return TCB_PTR_CTE_PTR(cap_thread_cap_get_capTCBPtr(cap), 0);

    case cap_zombie_cap:
        return CTE_PTR(cap_zombie_cap_get_capZombiePtr(cap));

    case cap_sched_context_cap:
        return SC_PTR(cap_sched_context_cap_get_capPtr(cap));

    case cap_reply_cap:
        return NULL;

    case cap_irq_control_cap:
        return NULL;

    case cap_irq_handler_cap:
        return NULL;

    case cap_sched_control_cap:
        return NULL;

    default:
        return cap_get_archCapPtr(cap);

    }
}


#endif
