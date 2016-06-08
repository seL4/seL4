/*
 * Copyright 2016, General Dynamics C4 Systems
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(GD_GPL)
 */

#ifndef BENCHMARK_TRACK_H
#define BENCHMARK_TRACK_H

#include <config.h>
#include <arch/benchmark.h>
#include <benchmark_track_types.h>
#include <arch/api/constants.h>
#include <machine/io.h>

/* we can fill the entire IPC buffer except for word 0, which
 * the kernel overwrites with the message tag */
#define MAX_IPC_BUFFER_STORAGE_SIZE (sizeof(seL4_IPCBuffer) - sizeof(seL4_Word))

#ifdef CONFIG_BENCHMARK_TRACK_KERNEL_ENTRIES
/**
 *  Calculate the maximum number of kernel entries that can be tracked,
 *  limited by the log buffer size. This is also the number of ksLog entries.
 *
 */
#define MAX_LOG_SIZE (seL4_LogBufferSize / \
             sizeof(benchmark_track_kernel_entry_t))

/**
 * The number of kernel entries that can fit into an IPC buffer.
 */
#define MAX_IPC_BUFFER_STORAGE (MAX_IPC_BUFFER_STORAGE_SIZE / sizeof(benchmark_track_kernel_entry_t))

extern timestamp_t ksEnter;
extern benchmark_track_kernel_entry_t *ksLog;
extern word_t ksLogIndex;
extern uint32_t ksLogIndexFinalized;
extern kernel_entry_t ksKernelEntry;

/**
 * @brief Fill in logging info for kernel entries
 *
 */
void benchmark_track_exit(void);

/**
 * @brief Dump entries to user's buffer.
 *
 * @param buffer user buffer
 * @param start_index start index of the kernel entries array
 * @param num_entries number of entries to dump starting from start_index
 *
 */
void benchmark_track_dump(
    benchmark_track_kernel_entry_t* buffer,
    word_t start_index,
    word_t num_entries
);

/**
 * @brief Start logging kernel entries
 *
 */
static inline void
benchmark_track_start(void)
{
    ksEnter = timestamp();
}

#endif /* CONFIG_BENCHMARK_TRACK_KERNEL_ENTRIES */
#endif /* BENCHMARK_TRACK_H */
