/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#include <config.h>
#include <assert.h>
#include <smp/lock.h>

#ifdef ENABLE_SMP_SUPPORT
compile_assert(BKL_not_padded, sizeof(big_kernel_lock) % EXCL_RES_GRANULE_SIZE == 0);

clh_lock_t big_kernel_lock;

BOOT_CODE void clh_lock_init(void)
{
    /* Check if linker honoured alignment */
    assert(((seL4_Word)&big_kernel_lock) % EXCL_RES_GRANULE_SIZE == 0);

    for (int i = 0; i < CONFIG_MAX_NUM_NODES; i++) {
        big_kernel_lock.request[i].state = CLHState_Pending;
        big_kernel_lock.node[i].myreq = &big_kernel_lock.request[i];
    }
    /* Initialize the CLH tail */
    big_kernel_lock.request[CONFIG_MAX_NUM_NODES].state = CLHState_Granted;
    big_kernel_lock.tail = &big_kernel_lock.request[CONFIG_MAX_NUM_NODES];
}

NO_INLINE void clh_lock_acquire_slowpath(bool_t irqPath, word_t cpu, clh_req_t *watch)
{
    /* Wait until predecessor finishes */
    while (watch->state != CLHState_Granted) {
        /* As we are in a loop we need to ensure that any loads of future iterations of the
         * loop are performed after this one */
        __atomic_thread_fence(__ATOMIC_ACQUIRE);
        if (clh_is_ipi_pending(cpu)) {
            /* we only handle irq_remote_call_ipi here as other type of IPIs
             * are async and could be delayed. 'handleIPI' may not return
             * based on value of the 'irqPath'. */
            handleIPI(CORE_IRQ_TO_IRQT(cpu, irq_remote_call_ipi), irqPath);
        }
        arch_pause();
    }
}

#endif /* ENABLE_SMP_SUPPORT */
