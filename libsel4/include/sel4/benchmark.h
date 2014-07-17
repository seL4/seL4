/*
 * Copyright 2014, NICTA
 *
 * This software may be distributed and modified according to the terms of
 * the BSD 2-Clause license. Note that NO WARRANTY is provided.
 * See "LICENSE_BSD2.txt" for details.
 *
 * @TAG(NICTA_BSD)
 */

#ifndef LIBSEL4_BENCHMARK
#define LIBSEL4_BENCHMARK

#ifdef CONFIG_BENCHMARK

/* entire ipc buffer except tag register (word 0) */
#define MAX_IPC_BUFFER (1024 - 1)

#include <sel4/sel4.h>
#include <stdint.h>

static inline void
seL4_BenchmarkDumpFullLog()
{
    uint32_t potential_size = seL4_BenchmarkLogSize();

    for (uint32_t j = 0; j < potential_size; j += MAX_IPC_BUFFER) {
        uint32_t chunk = potential_size - j;
        uint32_t requested = chunk > MAX_IPC_BUFFER ? MAX_IPC_BUFFER : chunk;
        uint32_t recorded = seL4_BenchmarkDumpLog(j, requested);
        for (uint32_t i = 0; i < recorded; i++) {
            printf("%u ", seL4_GetMR(i));
        }
        printf("\n");
        /* we filled the log buffer */
        if (requested != recorded) {
            printf("Dumped %u of %u potential logs\n", j + recorded, potential_size);
            return;
        }
    }

    /* logged amount was smaller than log buffer */
    printf("Dumped entire log, size %u\n", potential_size);
}

#endif /* CONFIG_BENCHMARK */
#endif /* LIBSEL4_BENCHMARK */

