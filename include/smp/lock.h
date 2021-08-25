/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#pragma once

#include <config.h>
#include <types.h>
#include <util.h>
#include <mode/machine.h>
#include <arch/model/statedata.h>
#include <smp/ipi.h>
#include <util.h>

#ifdef ENABLE_SMP_SUPPORT

/* CLH lock is FIFO lock for machines with coherent caches (coherent-FIFO lock).
 * See ftp://ftp.cs.washington.edu/tr/1993/02/UW-CSE-93-02-02.pdf */

typedef enum {
    CLHState_Granted = 0,
    CLHState_Pending
} clh_qnode_state_t;

typedef struct clh_qnode {
    clh_qnode_state_t value;

    PAD_TO_NEXT_CACHE_LN(sizeof(clh_qnode_state_t));
} clh_qnode_t;

typedef struct clh_qnode_p {
    clh_qnode_t *node;
    clh_qnode_t *next;
    /* This is the software IPI flag */
    word_t ipi;

    PAD_TO_NEXT_CACHE_LN(sizeof(clh_qnode_t *) +
                         sizeof(clh_qnode_t *) +
                         sizeof(word_t));
} clh_qnode_p_t;

typedef struct clh_lock {
    clh_qnode_t nodes[CONFIG_MAX_NUM_NODES + 1];
    clh_qnode_p_t node_owners[CONFIG_MAX_NUM_NODES];

    clh_qnode_t *head;
    PAD_TO_NEXT_CACHE_LN(sizeof(clh_qnode_t *));
} clh_lock_t;

extern clh_lock_t big_kernel_lock;
BOOT_CODE void clh_lock_init(void);

static inline bool_t FORCE_INLINE clh_is_ipi_pending(word_t cpu)
{
    return big_kernel_lock.node_owners[cpu].ipi == 1;
}

static inline void *sel4_atomic_exchange(void *ptr, bool_t
                                         irqPath, word_t cpu, int memorder)
{
    clh_qnode_t *prev;

    if (memorder == __ATOMIC_RELEASE || memorder == __ATOMIC_ACQ_REL) {
        __atomic_thread_fence(__ATOMIC_RELEASE);
    } else if (memorder == __ATOMIC_SEQ_CST) {
        __atomic_thread_fence(__ATOMIC_SEQ_CST);
    }

    while (!try_arch_atomic_exchange_rlx(&big_kernel_lock.head,
                                         (void *) big_kernel_lock.node_owners[cpu].node,
                                         (void **) &prev)) {
        if (clh_is_ipi_pending(cpu)) {
            /* we only handle irq_remote_call_ipi here as other type of IPIs
             * are async and could be delayed. 'handleIPI' may not return
             * based on value of the 'irqPath'. */
            handleIPI(CORE_IRQ_TO_IRQT(cpu, irq_remote_call_ipi), irqPath);
        }

        arch_pause();
    }

    if (memorder == __ATOMIC_ACQUIRE || memorder == __ATOMIC_ACQ_REL) {
        __atomic_thread_fence(__ATOMIC_ACQUIRE);
    } else if (memorder == __ATOMIC_SEQ_CST) {
        __atomic_thread_fence(__ATOMIC_SEQ_CST);
    }

    return prev;
}

static inline void FORCE_INLINE clh_lock_acquire(word_t cpu, bool_t irqPath)
{
    clh_qnode_t *prev;
    big_kernel_lock.node_owners[cpu].node->value = CLHState_Pending;

    prev = sel4_atomic_exchange(&big_kernel_lock.head, irqPath, cpu, __ATOMIC_ACQ_REL);

    big_kernel_lock.node_owners[cpu].next = prev;

    /* We do not have an __atomic_thread_fence here as this is already handled by the
     * atomic_exchange just above */
    while (big_kernel_lock.node_owners[cpu].next->value != CLHState_Granted) {
        /* As we are in a loop we need to ensure that any loads of future iterations of the
         * loop are performed after this one */
        __atomic_thread_fence(__ATOMIC_ACQUIRE);
        if (clh_is_ipi_pending(cpu)) {
            /* we only handle irq_remote_call_ipi here as other type of IPIs
             * are async and could be delayed. 'handleIPI' may not return
             * based on value of the 'irqPath'. */
            handleIPI(CORE_IRQ_TO_IRQT(cpu, irq_remote_call_ipi), irqPath);
            /* We do not need to perform a memory release here as we would have only modified
             * local state that we do not need to make visible */
        }
        arch_pause();
    }

    /* make sure no resource access passes from this point */
    __atomic_thread_fence(__ATOMIC_ACQUIRE);
}

static inline void FORCE_INLINE clh_lock_release(word_t cpu)
{
    /* make sure no resource access passes from this point */
    __atomic_thread_fence(__ATOMIC_RELEASE);

    big_kernel_lock.node_owners[cpu].node->value = CLHState_Granted;
    big_kernel_lock.node_owners[cpu].node =
        big_kernel_lock.node_owners[cpu].next;
}

static inline bool_t FORCE_INLINE clh_is_self_in_queue(void)
{
    return big_kernel_lock.node_owners[getCurrentCPUIndex()].node->value == CLHState_Pending;
}

#define NODE_LOCK(_irqPath) do {                         \
    clh_lock_acquire(getCurrentCPUIndex(), _irqPath);    \
} while(0)

#define NODE_UNLOCK do {                                 \
    clh_lock_release(getCurrentCPUIndex());              \
} while(0)

#define NODE_LOCK_IF(_cond, _irqPath) do {               \
    if((_cond)) {                                        \
        NODE_LOCK(_irqPath);                             \
    }                                                    \
} while(0)

#define NODE_UNLOCK_IF_HELD do {                         \
    if(clh_is_self_in_queue()) {                         \
        NODE_UNLOCK;                                     \
    }                                                    \
} while(0)

#else
#define NODE_LOCK(_irq) do {} while (0)
#define NODE_UNLOCK do {} while (0)
#define NODE_LOCK_IF(_cond, _irq) do {} while (0)
#define NODE_UNLOCK_IF_HELD do {} while (0)
#endif /* ENABLE_SMP_SUPPORT */

#define NODE_LOCK_SYS NODE_LOCK(false)
#define NODE_LOCK_IRQ NODE_LOCK(true)
#define NODE_LOCK_SYS_IF(_cond) NODE_LOCK_IF(_cond, false)
#define NODE_LOCK_IRQ_IF(_cond) NODE_LOCK_IF(_cond, true)

