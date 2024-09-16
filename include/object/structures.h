/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#pragma once

#include <config.h>
#include <api/types.h>
#include <stdint.h>
#include <arch/object/structures_gen.h>
#include <mode/types.h>
#include <sel4/macros.h>
#include <sel4/arch/constants.h>
#include <sel4/sel4_arch/constants.h>
#include <benchmark/benchmark_utilisation_.h>

enum irq_state {
    IRQInactive  = 0,
    IRQSignal    = 1,
    IRQTimer     = 2,
#ifdef ENABLE_SMP_SUPPORT
    IRQIPI       = 3,
#endif
    IRQReserved
};
typedef word_t irq_state_t;

typedef struct dschedule {
    dom_t domain;
    word_t length;
} dschedule_t;

enum asidSizeConstants {
    asidHighBits = seL4_NumASIDPoolsBits,
    asidLowBits = seL4_ASIDPoolIndexBits
};

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

// We would like the actual 'tcb' region (the portion that contains the tcb_t) of the tcb
// to be as large as possible, but it still needs to be aligned. As the TCB object contains
// two sub objects the largest we can make either sub object whilst preserving size alignment
// is half the total size. To halve an object size defined in bits we just subtract 1
//
// A diagram of a TCB kernel object that is created from untyped:
//  _______________________________________
// |     |             |                   |
// |     |             |                   |
// |cte_t|   unused    |       tcb_t       |
// |     |(debug_tcb_t)|                   |
// |_____|_____________|___________________|
// 0     a             b                   c
// a = tcbCNodeEntries * sizeof(cte_t)
// b = BIT(TCB_SIZE_BITS)
// c = BIT(seL4_TCBBits)
//
#define TCB_SIZE_BITS (seL4_TCBBits - 1)

#define TCB_CNODE_SIZE_BITS (TCB_CNODE_RADIX + seL4_SlotBits)
#define TCB_CNODE_RADIX     4
#define TCB_OFFSET          BIT(TCB_SIZE_BITS)

/* Generate a tcb_t or cte_t pointer from a tcb block reference */
#define TCB_PTR(r)       ((tcb_t *)(r))
#define TCB_CTE_PTR(r,i) (((cte_t *)(r))+(i))
#define TCB_REF(p)       ((word_t)(p))

/* Generate a cte_t pointer from a tcb_t pointer */
#define TCB_PTR_CTE_PTR(p,i) \
    (((cte_t *)((pptr_t)(p)&~MASK(seL4_TCBBits)))+(i))

#define SC_REF(p) ((word_t) (p))
#define SC_PTR(r) ((sched_context_t *) (r))

#define REPLY_REF(p) ((word_t) (p))
#define REPLY_PTR(r) ((reply_t *) (r))

#define WORD_PTR(r) ((word_t *)(r))
#define WORD_REF(p) ((word_t)(p))

#define ZombieType_ZombieTCB        BIT(wordRadix)
#define ZombieType_ZombieCNode(n)   ((n) & MASK(wordRadix))

static inline cap_t CONST Zombie_new(word_t number, word_t type, pptr_t ptr)
{
    word_t mask;

    if (type == ZombieType_ZombieTCB) {
        mask = MASK(TCB_CNODE_RADIX + 1);
    } else {
        mask = MASK(type + 1);
    }

    return cap_zombie_cap_new((ptr & ~mask) | (number & mask), type);
}

static inline word_t CONST cap_zombie_cap_get_capZombieBits(cap_t cap)
{
    word_t type = cap_zombie_cap_get_capZombieType(cap);
    if (type == ZombieType_ZombieTCB) {
        return TCB_CNODE_RADIX;
    }
    return ZombieType_ZombieCNode(type); /* cnode radix */
}

static inline word_t CONST cap_zombie_cap_get_capZombieNumber(cap_t cap)
{
    word_t radix = cap_zombie_cap_get_capZombieBits(cap);
    return cap_zombie_cap_get_capZombieID(cap) & MASK(radix + 1);
}

static inline pptr_t CONST cap_zombie_cap_get_capZombiePtr(cap_t cap)
{
    word_t radix = cap_zombie_cap_get_capZombieBits(cap);
    return cap_zombie_cap_get_capZombieID(cap) & ~MASK(radix + 1);
}

static inline cap_t CONST cap_zombie_cap_set_capZombieNumber(cap_t cap, word_t n)
{
    word_t radix = cap_zombie_cap_get_capZombieBits(cap);
    pptr_t ptr = cap_zombie_cap_get_capZombieID(cap) & ~MASK(radix + 1);
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

#ifdef CONFIG_KERNEL_MCS
    /* IPC buffer cap slot */
    tcbBuffer = 2,

    /* Fault endpoint slot */
    tcbFaultHandler = 3,

    /* Timeout endpoint slot */
    tcbTimeoutHandler = 4,
#else
    /* Reply cap slot */
    tcbReply = 2,

    /* TCB of most recent IPC sender */
    tcbCaller = 3,

    /* IPC buffer cap slot */
    tcbBuffer = 4,
#endif
    tcbCNodeEntries
};
typedef word_t tcb_cnode_index_t;

#include <arch/object/structures.h>

struct user_data {
    word_t words[BIT(seL4_PageBits) / sizeof(word_t)];
};
typedef struct user_data user_data_t;

struct user_data_device {
    word_t words[BIT(seL4_PageBits) / sizeof(word_t)];
};
typedef struct user_data_device user_data_device_t;

static inline word_t CONST wordFromVMRights(vm_rights_t vm_rights)
{
    return (word_t)vm_rights;
}

static inline vm_rights_t CONST vmRightsFromWord(word_t w)
{
    return (vm_rights_t)w;
}

static inline vm_attributes_t CONST vmAttributesFromWord(word_t w)
{
    vm_attributes_t attr;

    attr.words[0] = w;
    return attr;
}

#ifdef CONFIG_KERNEL_MCS
typedef struct sched_context sched_context_t;
typedef struct reply reply_t;
#endif

/* TCB: size >= 18 words + sizeof(arch_tcb_t) + 1 word on MCS (aligned to nearest power of 2) */
struct tcb {
    /* arch specific tcb state (including context)*/
    arch_tcb_t tcbArch;

    /* Thread state, 3 words */
    thread_state_t tcbState;

    /* Notification that this TCB is bound to. If this is set, when this TCB waits on
     * any sync endpoint, it may receive a signal from a Notification object.
     * 1 word*/
    notification_t *tcbBoundNotification;

    /* Current fault, 2 words */
    seL4_Fault_t tcbFault;

    /* Current lookup failure, 2 words */
    lookup_fault_t tcbLookupFailure;

    /* Domain, 1 byte (padded to 1 word) */
    dom_t tcbDomain;

    /*  maximum controlled priority, 1 byte (padded to 1 word) */
    prio_t tcbMCP;

    /* Priority, 1 byte (padded to 1 word) */
    prio_t tcbPriority;

#ifdef CONFIG_KERNEL_MCS
    /* scheduling context that this tcb is running on, if it is NULL the tcb cannot
     * be in the scheduler queues, 1 word */
    sched_context_t *tcbSchedContext;

    /* scheduling context that this tcb yielded to */
    sched_context_t *tcbYieldTo;
#else
    /* Timeslice remaining, 1 word */
    word_t tcbTimeSlice;

    /* Capability pointer to thread fault handler, 1 word */
    cptr_t tcbFaultHandler;
#endif

    /* userland virtual address of thread IPC buffer, 1 word */
    word_t tcbIPCBuffer;

#ifdef ENABLE_SMP_SUPPORT
    /* cpu ID this thread is running on, 1 word */
    word_t tcbAffinity;
#endif /* ENABLE_SMP_SUPPORT */

    /* Previous and next pointers for scheduler queues , 2 words */
    struct tcb *tcbSchedNext;
    struct tcb *tcbSchedPrev;
    /* Preivous and next pointers for endpoint and notification queues, 2 words */
    struct tcb *tcbEPNext;
    struct tcb *tcbEPPrev;

#ifdef CONFIG_BENCHMARK_TRACK_UTILISATION
    /* 16 bytes (12 bytes aarch32) */
    benchmark_util_t benchmark;
#endif
};
typedef struct tcb tcb_t;

#ifdef CONFIG_DEBUG_BUILD
/* This debug_tcb object is inserted into the 'unused' region of a TCB object
   for debug build configurations. */
struct debug_tcb {

    /* Pointers for list of all tcbs that is maintained
     * when CONFIG_DEBUG_BUILD is enabled, 2 words */
    struct tcb *tcbDebugNext;
    struct tcb *tcbDebugPrev;
    /* Use any remaining space for a thread name */
    char tcbName[];

};
typedef struct debug_tcb debug_tcb_t;

#define TCB_PTR_DEBUG_PTR(p) ((debug_tcb_t *)TCB_PTR_CTE_PTR(p,tcbArchCNodeEntries))
#endif /* CONFIG_DEBUG_BUILD */

#ifdef CONFIG_KERNEL_MCS
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

    /* amount of ticks this sc has been scheduled for since seL4_SchedContext_Consumed
     * was last called or a timeout exception fired */
    ticks_t scConsumed;

    /* core this scheduling context provides time for - 0 if uniprocessor */
    word_t scCore;

    /* thread that this scheduling context is bound to */
    tcb_t *scTcb;

    /* if this is not NULL, it points to the last reply object that was generated
     * when the scheduling context was passed over a Call */
    reply_t *scReply;

    /* notification this scheduling context is bound to */
    notification_t *scNotification;

    /* data word that is sent with timeout faults that occur on this scheduling context */
    word_t scBadge;

    /* thread that yielded to this scheduling context */
    tcb_t *scYieldFrom;

    /* Amount of refills this sc tracks */
    word_t scRefillMax;
    /* Index of the head of the refill circular buffer */
    word_t scRefillHead;
    /* Index of the tail of the refill circular buffer */
    word_t scRefillTail;

    /* Whether to apply constant-bandwidth/sliding-window constraint
     * rather than only sporadic server constraints */
    bool_t scSporadic;
};

struct reply {
    /* TCB pointed to by this reply object. This pointer reflects two possible relations, depending
     * on the thread state.
     *
     * ThreadState_BlockedOnReply: this tcb is the caller that is blocked on this reply object,
     * ThreadState_BlockedOnRecv: this tcb is the callee blocked on an endpoint with this reply object.
     *
     * The back pointer for this TCB is stored in the thread state.*/
    tcb_t *replyTCB;

    /* 0 if this is the start of the call chain, or points to the
     * previous reply object in a call chain */
    call_stack_t replyPrev;

    /* Either a scheduling context if this reply object is the head of the call chain
     * (the last caller before the server) or another reply object. 0 if no scheduling
     * context was passed along the call chain */
    call_stack_t replyNext;

    /* Unused, explicit padding to make struct size the correct power of 2. */
    word_t padding;
};
#endif

/* Ensure object sizes are sane */
compile_assert(cte_size_sane, sizeof(cte_t) == BIT(seL4_SlotBits))
compile_assert(tcb_cte_size_sane, TCB_CNODE_SIZE_BITS <= TCB_SIZE_BITS)
compile_assert(tcb_size_sane,
               BIT(TCB_SIZE_BITS) >= sizeof(tcb_t))
compile_assert(tcb_size_not_excessive,
               BIT(TCB_SIZE_BITS - 1) < sizeof(tcb_t))
compile_assert(ep_size_sane, sizeof(endpoint_t) == BIT(seL4_EndpointBits))
compile_assert(notification_size_sane, sizeof(notification_t) == BIT(seL4_NotificationBits))

/* Check the IPC buffer is the right size */
compile_assert(ipc_buf_size_sane, sizeof(seL4_IPCBuffer) == BIT(seL4_IPCBufferSizeBits))
#ifdef CONFIG_KERNEL_MCS
compile_assert(sc_core_size_sane, (sizeof(sched_context_t) + MIN_REFILLS *sizeof(refill_t) ==
                                   seL4_CoreSchedContextBytes))
compile_assert(reply_size_sane, sizeof(reply_t) == BIT(seL4_ReplyBits))
compile_assert(refill_size_sane, (sizeof(refill_t) == seL4_RefillSizeBytes))
#endif

/* helper functions */

static inline word_t CONST
isArchCap(cap_t cap)
{
    return (cap_get_capType(cap) % 2);
}

