/*
 * Copyright 2026 Indan Zupancic
 * SPDX-License-Identifier: GPL-2.0-only
 *
 * seL4 IPI test for GenMC, a Generic Model Checker for Concurrent Programs.
 *
 * See smp-bkl-genmc.c for more information.
 *
 * Two IPI callers and two targets:
 */
#define N 4
#include "wrappers.h"

#include "../../include/smp/ipi.h"
#include "../../include/smp/lock.h"
#include "../../src/smp/lock.c"
#include "../../src/smp/ipi.c"

_Thread_local int num_ipis_received;
int handled_ipi_call;
int a, b, c, w;

/* We can't emulate interrupts to wake tasks up easily, instead spin on clh_is_ipi_pending(). */
void arch_ipi_send_target(irq_t irq, word_t cpu) {}

void arch_handleRemoteCall(IpiRemoteCall_t call, word_t arg0, word_t arg1, word_t arg2)
{
    /* Only write global data if it is a one to one IPI: */
    if (call == IpiRemoteCall_Stall) {
        handled_ipi_call = call;
        a = arg0;
        b = arg1;
        c = arg2;
    }
    num_ipis_received++;
}

void do_ipi(IpiRemoteCall_t call, unsigned int target_mask)
{
    int x = __VERIFIER_nondet_int();
    int y = x + 1;
    int z = x + 2;

    /* Check we don't have a bug in the test code: */
    assert(!clh_is_ipi_pending(CurrentCPUIndex));
    clh_lock_acquire(true);
    w = x;
    assert(w == x);
    ipi_remote_call(call, x, y, z, target_mask);

    /* Check that remote "core" handled the IPI: */
    if (call == IpiRemoteCall_Stall) {
        assert(handled_ipi_call == IpiRemoteCall_Stall);
        assert(a == x);
        assert(b == y);
        assert(c == z);
    }
    clh_lock_release();
}

void *caller(void *arg)
{
    CurrentCPUIndex = (ptrdiff_t)arg;

    /* Test broadcasting IPIs to all targets: */
    __VERIFIER_output("Send broadcast IPI");
    do_ipi(IpiRemoteCall_Test, (1 << 2) | (1 << 3));
    return 0;
}

void *caller2(void *arg)
{
    CurrentCPUIndex = (ptrdiff_t)arg;

    /* Test individual IPIs to each target: */
    __VERIFIER_output("Send IPI to 3");
    do_ipi(IpiRemoteCall_Stall, 1 << 3);
    __VERIFIER_output("Send IPI to 2");
    do_ipi(IpiRemoteCall_Stall, 1 << 2);
    return 0;
}

void *target(void *arg)
{
    unsigned int cpu = CurrentCPUIndex = (ptrdiff_t)arg;

    /* We can handle multiple IPIs within one clh_lock_acquire(). */
    while (num_ipis_received < 2) {
        /* Wait for an IPI to be pending for this target: */
        while (!clh_is_ipi_pending(cpu));
        /* Now handle the IPI in clh_lock_acquire(): */
        clh_lock_acquire(true);
        clh_lock_release();
    }
    return 0;
}

int main(void)
{
    pthread_t t[N];

    clh_lock_init();

    if (pthread_create(&t[0], NULL, caller, 0) != 0) {
        return 1;
    }
    if (pthread_create(&t[1], NULL, caller2, (void *)1) != 0) {
        return 1;
    }
    for (unsigned long i = 2; i < N; ++i) {
        if (pthread_create(&t[i], NULL, target, (void *)i) != 0) {
            return 1;
        }
    }
    for (int i = 0; i < N; ++i) {
        pthread_join(t[i], NULL);
    }
    return 0;
}
