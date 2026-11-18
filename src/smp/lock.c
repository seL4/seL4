/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#ifndef GENMC_TEST
#include <config.h>
#include <assert.h>
#include <smp/lock.h>
#endif

#ifdef ENABLE_SMP_SUPPORT
compile_assert(BKL_not_padded, sizeof(big_kernel_lock) % EXCL_RES_GRANULE_SIZE == 0);

clh_lock_t big_kernel_lock;

BOOT_CODE void clh_lock_init(void)
{
    /* Check if linker honoured alignment */
    assert(((word_t)&big_kernel_lock) % EXCL_RES_GRANULE_SIZE == 0);

    for (int i = 0; i < CONFIG_MAX_NUM_NODES; i++) {
        big_kernel_lock.request[i].state = CLHState_Pending;
        big_kernel_lock.node[i].myreq = &big_kernel_lock.request[i];
    }
    /* Initialize the CLH tail */
    big_kernel_lock.request[CONFIG_MAX_NUM_NODES].state = CLHState_Granted;
    big_kernel_lock.tail = &big_kernel_lock.request[CONFIG_MAX_NUM_NODES];
    big_kernel_lock.ipi.call = IpiNumArchRemoteCall;
}

NO_INLINE void clh_lock_acquire_slowpath(bool_t irqPath, word_t cpu, clh_req_t *watch)
{
    /* Wait until predecessor finishes */
    do {
        if (clh_is_ipi_pending(cpu)) {
            ipi_handle_call(irqPath);
        }
        arch_pause();
    } while (__atomic_load_n(&watch->state, __ATOMIC_ACQUIRE) != CLHState_Granted);
}

#endif /* ENABLE_SMP_SUPPORT */
