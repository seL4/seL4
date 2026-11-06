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
 * See https://dada.cs.washington.edu/research/tr/1993/02/UW-CSE-93-02-02.pdf
 */
typedef enum {
    CLHState_Granted = 0,
    CLHState_Pending
} clh_req_state_t;

/* Lock request */
typedef struct clh_req {
    clh_req_state_t state;
} ALIGN(L1_CACHE_LINE_SIZE) clh_req_t;

/* Our node (called "Process" in the paper) */
typedef struct clh_node {
    clh_req_t *watch; // Used by predecessor to grant the lock to us.
    clh_req_t *myreq; // Used to grant the lock to our successor.
    /* This is the software blocking call IPI flag */
    word_t ipi;
} ALIGN(L1_CACHE_LINE_SIZE) clh_node_t;

typedef struct clh_lock {
    clh_req_t request[CONFIG_MAX_NUM_NODES + 1];
    clh_node_t node[CONFIG_MAX_NUM_NODES];

    clh_req_t *tail;

    /* Global IPI state */
    ipi_state_t ipi;
} ALIGN(EXCL_RES_GRANULE_SIZE) clh_lock_t;

extern clh_lock_t big_kernel_lock;

BOOT_CODE void clh_lock_init(void);
void clh_lock_acquire_slowpath(bool_t irqPath, word_t cpu, clh_req_t *watch);

static inline bool_t FORCE_INLINE clh_is_ipi_pending(word_t cpu)
{
    /* Asssure IPI data is accessed only when this flag is set */
    return __atomic_load_n(&big_kernel_lock.node[cpu].ipi, __ATOMIC_ACQUIRE);
}

static inline void FORCE_INLINE clh_lock_acquire(bool_t irqPath)
{
    word_t cpu = getCurrentCPUIndex();
    clh_node_t *node = &big_kernel_lock.node[cpu];
    clh_req_t *watch;

    /* Enqueue our request */
    watch = __atomic_exchange_n(&big_kernel_lock.tail, node->myreq, __ATOMIC_ACQ_REL);

    node->watch = watch;
    if (watch->state != CLHState_Granted) {
        clh_lock_acquire_slowpath(irqPath, cpu, watch);
    }
    /* make sure no resource access passes from this point */
    __atomic_thread_fence(__ATOMIC_ACQUIRE);
}

static inline void FORCE_INLINE clh_lock_release(void)
{
    clh_node_t *node = &big_kernel_lock.node[getCurrentCPUIndex()];

    /* make sure no resource access passes from this point */
    __atomic_thread_fence(__ATOMIC_RELEASE);

    /* Pass lock to successor */
    node->myreq->state = CLHState_Granted;
    /* Take ownership of watched request, to use next time we take the lock */
    node->myreq = node->watch;
    /* Tell future successor to wait */
    node->myreq->state = CLHState_Pending;
}

#define NODE_LOCK(_irqPath) do {                         \
    clh_lock_acquire(_irqPath);                          \
} while(0)

#define NODE_UNLOCK do {                                 \
    clh_lock_release();                                  \
} while(0)

#else
#define NODE_LOCK(_irq) do {} while (0)
#define NODE_UNLOCK do {} while (0)
#endif /* ENABLE_SMP_SUPPORT */

#define NODE_LOCK_SYS NODE_LOCK(false)
#define NODE_LOCK_IRQ NODE_LOCK(true)

