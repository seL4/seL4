/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#pragma once

#include <config.h> // for arch/api/syscall.h
#include <machine.h>
#include <api/failures.h>
#include <model/statedata.h>
#include <kernel/vspace.h>
#include <arch/api/syscall.h>
#include <api/debug.h>

#define TIME_ARG_SIZE (sizeof(ticks_t) / sizeof(word_t))

#ifdef CONFIG_KERNEL_MCS
#define MCS_DO_IF_BUDGET(_block) \
    updateTimestamp(); \
    if (likely(checkBudgetRestart())) { \
        _block \
    }
#else
#define MCS_DO_IF_BUDGET(_block) \
    { \
        _block \
    }
#endif

exception_t handleSyscall(syscall_t syscall);
exception_t handleInterruptEntry(void);
exception_t handleUnknownSyscall(word_t w);
exception_t handleUserLevelFault(word_t w_a, word_t w_b);
exception_t handleVMFaultEvent(vm_fault_type_t vm_faultType);

static inline word_t PURE getSyscallArg(word_t i, word_t *ipc_buffer)
{
    if (i < n_msgRegisters) {
        return getRegister(NODE_STATE(ksCurThread), msgRegisters[i]);
    }

    assert(ipc_buffer != NULL);
    return ipc_buffer[i + 1];
}

extern extra_caps_t current_extra_caps;

