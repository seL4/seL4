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
#include <arch/api/constants.h>
#include <machine/io.h>

#if (defined CONFIG_BENCHMARK_TRACK_SYSCALLS || defined DEBUG)

/* we can fill the entire IPC buffer except for word 0, which
 * the kernel overwrites with the message tag */
#define MAX_IPC_BUFFER_STORAGE (sizeof(seL4_IPCBuffer) - sizeof(seL4_Word))

/* the following code can be used at any point in the kernel
 * to determine detail about the kernel entry point */
typedef enum {
    Entry_Interrupt,
    Entry_UnknownSyscall,
    Entry_UserLevelFault,
    Entry_VMFault,
    Entry_Syscall
} entry_type_t;

/**
 * @brief Kernel entry logging
 *
 * Encapsulates useful info about the cause of the kernel entry
 */
typedef struct PACKED kernel_entry {
    word_t path: 3;
    union {
        /* TODO track interrupts */
        struct {
            word_t word: 29;
        };
        /* Tracked kernel entry info filled from outside this file */
        struct {
            word_t syscall_no: 3;
            word_t cap_type: 5;
            word_t is_fastpath: 1;
            word_t invocation_tag: 20;
        };
    };
} kernel_entry_t;

extern kernel_entry_t ksKernelEntry;
#endif /* CONFIG_BENCHMARK_TRACK_SYSCALLS || DEBUG */

#ifdef CONFIG_BENCHMARK_TRACK_SYSCALLS

typedef struct benchmark_syscall_log_entry {
    timestamp_t  start_time;
    uint32_t  duration;
    kernel_entry_t entry;
} benchmark_track_syscall_invocation_t;

extern timestamp_t ksEnter;
extern benchmark_track_syscall_invocation_t *ksLog;
extern word_t ksIndex;

/**
 *  Calculate the maximum number of invocation entries that can be tracked,
 *  limited by the log buffer size. This is also the number of ksLog entries.
 *
 */
#define MAX_SYSCALL_INVOCATIONS_ENTRIES (seL4_LogBufferSize / \
             sizeof(benchmark_track_syscall_invocation_t))

/**
 * The number of invocation entries that can fit into an IPC buffer.
 */
#define MAX_IPC_LOG_ENTRIES (MAX_IPC_BUFFER_STORAGE / sizeof(benchmark_track_syscall_invocation_t))

/**
 * @brief Fill in logging info for a syscall invocation
 *
 */
void benchmark_track_syscall_exit(void);

/**
 * @brief Dump entries to user's IPC buffer
 *
 * @param buffer the user IPC buffer
 * @param start_index start index of the invocations array
 * @param num_entries number of entries to dump starting from start_index
 *
 */
void benchmark_track_syscall_dump(
    word_t* buffer,
    word_t start_index,
    word_t num_entries
);

/**
 * @brief Start logging this syscall invocation
 *
 */
static inline void
benchmark_track_syscall_start(void)
{
    ksEnter = timestamp();
}

/**
 * @brief Return the number of invocations for a tracked system call
 *
 * @retval number of tracked system call invocations.
 */
static inline word_t
benchmark_track_syscall_invocations_num(void)
{
    return (word_t) ksIndex;
}

/**
 * @brief Reset the counter for all tracked system calls
 *
 * reset starts tracking as well.
 */
static inline void
benchmark_track_syscalls_reset(void)
{
    /* Reset tracking */
    ksIndex = 0;
}

#endif /* CONFIG_BENCHMARK_TRACK_SYSCALLS */

#endif /* BENCHMARK_TRACK_H */
