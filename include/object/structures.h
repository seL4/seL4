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

#include <config.h>
#include <api/types.h>
#include <stdint.h>
#include <arch/object/structures_gen.h>
#include <api/macros.h>
#include <arch/api/constants.h>
#include <benchmark/benchmark_utilisation_.h>

enum irq_state {
    IRQInactive  = 0,
    IRQSignal    = 1,
    IRQTimer     = 2,
#if CONFIG_MAX_NUM_NODES > 1
    IRQIPI       = 3,
#endif
    IRQReserved
};
typedef word_t irq_state_t;

typedef struct dschedule {
    dom_t domain;
    word_t length;
} dschedule_t;

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

#define TCB_SIZE_BITS (TCB_CNODE_RADIX + seL4_SlotBits)
#define TCB_CNODE_RADIX     4
#define TCB_OFFSET          (1 << (TCB_SIZE_BITS))

/* Generate a tcb_t or cte_t pointer from a tcb block reference */
#define TCB_PTR(r)       ((tcb_t *)(r))
#define TCB_CTE_PTR(r,i) (((cte_t *)(r))+(i))
#define TCB_REF(p)       ((word_t)(p))

/* Generate a cte_t pointer from a tcb_t pointer */
#define TCB_PTR_CTE_PTR(p,i) \
    (((cte_t *)((word_t)(p)&~MASK(seL4_TCBBits)))+(i))

#define SC_REF(p) ((word_t) (p))
#define SC_PTR(r) ((sched_context_t *) (r))

#define REPLY_REF(p) ((word_t) (p))
#define REPLY_PTR(r) ((reply_t *) (r))

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
#ifdef CONFIG_VTX
    ThreadState_RunningVM,
#endif
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

    /* IPC buffer cap slot */
    tcbBuffer = 2,

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

typedef struct sched_context sched_context_t;
typedef struct reply reply_t;

/* TCB: size 64 bytes + sizeof(arch_tcb_t) (aligned to nearest power of 2) */
struct tcb {
    /* arch specific tcb state (including context)*/
    arch_tcb_t tcbArch;

    /* Thread state, 12 bytes */
    thread_state_t tcbState;

    /* Notification that this TCB is bound to. If this is set, when this TCB waits on
     * any sync endpoint, it may receive a signal from a Notification object.
     * 4 bytes*/
    notification_t *tcbBoundNotification;

    /* if tcb is in a call, pointer to the reply object, 4 bytes */
    reply_t *tcbReply;

    /* Previous and next pointers for scheduler queues , 8 bytes */
    struct tcb* tcbSchedNext;
    struct tcb* tcbSchedPrev;
    /* Preivous and next pointers for endpoint and notification queues, 8 bytes */
    struct tcb* tcbEPNext;
    struct tcb* tcbEPPrev;

    /* Current fault, 8 bytes */
    seL4_Fault_t tcbFault;

    /* Current lookup failure, 8 bytes */
    lookup_fault_t tcbLookupFailure;

    /* Domain, 1 byte (packed to 4) */
    dom_t tcbDomain;

    /*  maximum controlled priorioty, 1 byte (packed to 4) */
    prio_t tcbMCP;

    /* Priority, 1 byte (packed to 4) */
    prio_t tcbPriority;

    /* scheduling context that this tcb is running on, if it is NULL the tcb cannot
     * be in the scheduler queues */
    sched_context_t *tcbSchedContext;

    /* Capability pointer to thread fault handler, 4 bytes */
    cptr_t tcbFaultHandler;

    /* userland virtual address of thread IPC buffer, 4 bytes */
    word_t tcbIPCBuffer;

#if CONFIG_MAX_NUM_NODES > 1
    /* cpu ID this thread last ran on */
    word_t tcbAffinity;
#endif /* CONFIG_MAX_NUM_NODES */

#ifdef CONFIG_BENCHMARK_TRACK_UTILISATION
    benchmark_util_t benchmark;
#endif

#ifdef CONFIG_DEBUG_BUILD
    /* Use any remaining space for a thread name */
    char tcbName[];
#endif /* CONFIG_DEBUG_BUILD */
};
typedef struct tcb tcb_t;

typedef struct refill {
    /* Absolute timestamp from when this refill can be used */
    ticks_t rTime;
    /* Amount of ticks that can be used from this refill */
    ticks_t rAmount;
} refill_t;

#define MIN_REFILLS 2u

struct sched_context {
    /* period for this sc -- controls rate at which budget is replenished */
    ticks_t scPeriod;

    /* core this scheduling context provides time for - 0 if uniprocessor */
    word_t scCore;

    /* thread that this scheduling context is bound to */
    tcb_t *scTcb;

    /* if this is not NULL, it points to the last reply object that was generated
     * when the scheduling context was passed over a Call */
    reply_t *scReply;

    /* notification this scheduling context is bound to
     * (scTcb and scNotification cannot be set at the same time) */
    notification_t *scNotification;

    /* Amount of refills this sc tracks */
    word_t scRefillMax;
    /* Index of the head of the refill circular buffer */
    word_t scRefillHead;
    /* Index of the tail of the refill circular buffer */
    word_t scRefillTail;
};

struct reply {
    /* the caller that is blocked on this reply object */
    tcb_t *replyCaller;

    /* 0 if this is the start of the call chain, or points to the
     * previous reply object in a call chain */
    call_stack_t replyPrev;

    /* Either a scheduling context if this reply object is the head of the call chain
     * (the last caller before the server) or another reply object. 0 if no scheduling
     * context was passed along the call chain */
    call_stack_t replyNext;
};

/* Ensure object sizes are sane */
compile_assert(cte_size_sane, sizeof(cte_t) <= (1 << seL4_SlotBits))
compile_assert(tcb_size_sane,
               (1 << (TCB_SIZE_BITS)) + sizeof(tcb_t) <= (1 << seL4_TCBBits))
compile_assert(ep_size_sane, sizeof(endpoint_t) <= (1 << seL4_EndpointBits))
compile_assert(notification_size_sane, sizeof(notification_t) <= (1 << seL4_NotificationBits))
compile_assert(sc_size_sane, (sizeof(sched_context_t) + MIN_REFILLS * sizeof(refill_t)) <= (1 << seL4_MinSchedContextBits))
compile_assert(sc_core_size_sane, (sizeof(sched_context_t) + MIN_REFILLS * sizeof(refill_t) <= seL4_CoreSchedContextBytes))
compile_assert(refill_size_sane, (sizeof(refill_t) == seL4_RefillSizeBytes))
compile_assert(reply_size_sane, sizeof(reply_t) <= (1 << seL4_ReplyBits))

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
        return seL4_EndpointBits;

    case cap_notification_cap:
        return seL4_NotificationBits;

    case cap_cnode_cap:
        return cap_cnode_cap_get_capCNodeRadix(cap) + seL4_SlotBits;

    case cap_thread_cap:
        return seL4_TCBBits;

    case cap_zombie_cap: {
        word_t type = cap_zombie_cap_get_capZombieType(cap);
        if (type == ZombieType_ZombieTCB) {
            return seL4_TCBBits;
        }
        return ZombieType_ZombieCNode(type) + seL4_SlotBits;
    }

    case cap_null_cap:
        return 0;

    case cap_domain_cap:
        return 0;

    case cap_reply_cap:
        return seL4_ReplyBits;

    case cap_irq_control_cap:
    case cap_sched_control_cap:
        return 0;

    case cap_irq_handler_cap:
        return 0;

    case cap_sched_context_cap:
        return cap_sched_context_cap_get_capSCSizeBits(cap);

    default:
        return cap_get_archCapSizeBits(cap);
    }

}

/* Returns whether or not this capability has memory associated
 * with it or not. Refering to this as 'being physical' is to
 * match up with the haskell and abstract specifications */
static inline bool_t CONST
cap_get_capIsPhysical(cap_t cap)
{
    cap_tag_t ctag;

    ctag = cap_get_capType(cap);

    switch (ctag) {
    case cap_untyped_cap:
        return true;

    case cap_endpoint_cap:
        return true;

    case cap_notification_cap:
        return true;

    case cap_cnode_cap:
        return true;

    case cap_thread_cap:
    case cap_sched_context_cap:
        return true;

    case cap_zombie_cap:
        return true;

    case cap_domain_cap:
        return false;

    case cap_reply_cap:
        return true;

    case cap_irq_control_cap:
    case cap_sched_control_cap:
        return false;

    case cap_irq_handler_cap:
        return false;

    default:
        return cap_get_archCapIsPhysical(cap);
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

    case cap_domain_cap:
        return NULL;

    case cap_reply_cap:
        return REPLY_PTR(cap_reply_cap_get_capReplyPtr(cap));

    case cap_irq_control_cap:
    case cap_sched_control_cap:
        return NULL;

    case cap_irq_handler_cap:
        return NULL;

    case cap_sched_context_cap:
        return SC_PTR(cap_sched_context_cap_get_capSCPtr(cap));

    default:
        return cap_get_archCapPtr(cap);

    }
}


#endif
