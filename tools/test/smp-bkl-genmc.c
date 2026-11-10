/*
 * Copyright 2026 Indan Zupancic
 * SPDX-License-Identifier: GPL-2.0-only
 *
 * seL4 big kernel lock test for GenMC, a Generic Model Checker for Concurrent Programs.
 *
 * This checks the big kernel lock only, without IPI.
 *
 * Compile flags should mimic kernel build options, test with:
 *  CFLAGS="-O2 -Wall -mno-mmx -mno-sse -mno-sse2 -mno-3dnow -std=c99 -Wno-typedef-redefinition"
 *  genmc -- $CFLAGS tools/test/smp-bkl-genmc.c
 *
 * Or, when using the genmc/genmc Docker image:
 *  docker run --init --mount type=bind,source=/path/to/seL4,target=/root/seL4,readonly \
 *  genmc/genmc genmc/RelWithDebInfo/bin/genmc -- $CFLAGS seL4/tools/test/smp-bkl-genmc.c
 *
 * Check lock with three "cores", that is enough to test all states:
 * Task "running", owning the lock and queued on the lock.
 */
#define N 3
#include "wrappers.h"

typedef struct {
    int call;
} ipi_state_t;

static inline void ipi_handle_call(bool_t irqPath) {}

#include "../../include/smp/lock.h"
#include "../../src/smp/lock.c"

int a;

void *thread(void *arg)
{
    CurrentCPUIndex = (ptrdiff_t)arg;
    unsigned int x = CurrentCPUIndex + 1;

    /* Test concurrent same thread lock-unlock-lock-unlock sequences: */
    for (int i = 0; i < 2; ++i) {
        clh_lock_acquire(true);
        a = x;
        /* If locking works as intended, then no race is possible: */
        assert(a == x);
        clh_lock_release();
    }
    return 0;
}

int main(void)
{
    pthread_t t[N];

    clh_lock_init();

    for (unsigned long i = 0; i < N; ++i) {
        if (pthread_create(&t[i], NULL, thread, (void *)i) != 0) {
            return 1;
        }
    }
    for (int i = 0; i < N; ++i) {
        pthread_join(t[i], NULL);
    }
    return 0;
}
