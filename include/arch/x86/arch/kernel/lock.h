/*
 * Copyright 2016, Data61
 * Commonwealth Scientific and Industrial Research Organisation (CSIRO)
 * ABN 41 687 119 230.
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(D61_GPL)
 */

#pragma once

#include <config.h>
#include <types.h>
#include <util.h>
#include <mode/machine.h>
#include <arch/model/statedata.h>

#if CONFIG_MAX_NUM_NODES > 1

/* CLH lock is FIFO lock for machines with coherent caches (coherent-FIFO lock).
 * See ftp://ftp.cs.washington.edu/tr/1993/02/UW-CSE-93-02-02.pdf */

typedef enum {
    CLHState_Granted = 0,
    CLHState_Pending
} clh_qnode_state_t;

typedef struct clh_qnode {
    volatile clh_qnode_state_t value;

    PAD_TO_NEXT_CACHE_LN(sizeof(clh_qnode_state_t));
} clh_qnode_t;

typedef struct clh_qnode_p {
    volatile clh_qnode_t *node;
    volatile clh_qnode_t *next;
    
    PAD_TO_NEXT_CACHE_LN(sizeof(clh_qnode_t *) + sizeof(clh_qnode_t *));
} clh_qnode_p_t;

typedef struct clh_lock {
    volatile clh_qnode_t nodes[CONFIG_MAX_NUM_NODES + 1];
    volatile clh_qnode_p_t node_owners[CONFIG_MAX_NUM_NODES];

    volatile clh_qnode_t *head;
    PAD_TO_NEXT_CACHE_LN(sizeof(clh_qnode_t *));
} clh_lock_t;

extern clh_lock_t big_kernel_lock;
BOOT_CODE void clh_lock_init(void);

static inline void FORCE_INLINE
clh_lock_acquire(word_t cpu)
{
    volatile clh_qnode_t *prev;
    big_kernel_lock.node_owners[cpu].node->value = CLHState_Pending;

    /* rely on the full barrier implied by the GCC builtin*/
    prev = __sync_lock_test_and_set(&big_kernel_lock.head,
                                    big_kernel_lock.node_owners[cpu].node);
    big_kernel_lock.node_owners[cpu].next = prev;

    while (big_kernel_lock.node_owners[cpu].next->value != CLHState_Granted) {
        asm volatile("pause");
    }

    /* make sure no resource access passes from this point */
    asm volatile("" ::: "memory");
}

static inline void FORCE_INLINE
clh_lock_release(word_t cpu)
{
    /* make sure no resource access passes from this point,
     * no need to have any full barrier due x86 TSO memory ordering */
    asm volatile("" ::: "memory");

    big_kernel_lock.node_owners[cpu].node->value = CLHState_Granted;
    big_kernel_lock.node_owners[cpu].node =
        big_kernel_lock.node_owners[cpu].next;
}

static inline clh_qnode_state_t FORCE_INLINE
clh_lock_test(void)
{
    return big_kernel_lock.head->value;
}

#define NODE_LOCK do {                          \
    clh_lock_acquire(getCurrentCPUIndex());     \
    } while(0)

#define NODE_UNLOCK do {                        \
    clh_lock_release(getCurrentCPUIndex());     \
    } while(0)

#define LOCK_TEST do {                          \
    clh_lock_test()                             \
    } while(0)

#else

#define NODE_LOCK do {} while (0)
#define NODE_UNLOCK do {} while (0)
#define LOCK_TEST do {} while (0)

#endif
