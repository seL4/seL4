/*
 * Copyright 2016, General Dynamics C4 Systems
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#pragma once

#include <config.h>
#include <arch/benchmark.h>
#include <sel4/benchmark_track_types.h>
#include <sel4/arch/constants.h>
#include <machine/io.h>
#include <kernel/cspace.h>
#include <model/statedata.h>
#include <mode/machine.h>

#if defined(CONFIG_DEBUG_BUILD) || defined(CONFIG_BENCHMARK_TRACK_KERNEL_ENTRIES)
#define TRACK_KERNEL_ENTRIES 1
extern kernel_entry_t ksKernelEntry;
#ifdef CONFIG_BENCHMARK_TRACK_KERNEL_ENTRIES
/**
 *  Calculate the maximum number of kernel entries that can be tracked,
 *  limited by the log buffer size. This is also the number of ksLog entries.
 *
 */
#define MAX_LOG_SIZE (seL4_LogBufferSize / \
             sizeof(benchmark_track_kernel_entry_t))

extern timestamp_t ksEnter;
extern seL4_Word ksLogIndex;
extern seL4_Word ksLogIndexFinalized;

/**
 * @brief Fill in logging info for kernel entries
 *
 */
void benchmark_track_exit(void);

/**
 * @brief Start logging kernel entries
 *
 */
static inline void benchmark_track_start(void)
{
    ksEnter = timestamp();
}
#endif /* CONFIG_BENCHMARK_TRACK_KERNEL_ENTRIES */

static inline void benchmark_debug_syscall_start(word_t cptr, word_t msgInfo, word_t syscall)
{
    seL4_MessageInfo_t info = messageInfoFromWord_raw(msgInfo);
    lookupCapAndSlot_ret_t lu_ret = lookupCapAndSlot(NODE_STATE(ksCurThread), cptr);
    ksKernelEntry.path = Entry_Syscall;
    ksKernelEntry.syscall_no = -syscall;
    ksKernelEntry.cap_type = cap_get_capType(lu_ret.cap);
    ksKernelEntry.invocation_tag = seL4_MessageInfo_get_label(info);
}
#endif

