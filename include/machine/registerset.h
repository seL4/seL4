/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#pragma once

#include <util.h>
#include <arch/types.h>
#include <arch/machine/registerset.h>
#include <arch/object/structures.h>

typedef enum {
    MessageID_Syscall,
    MessageID_Exception,
#ifdef CONFIG_KERNEL_MCS
    MessageID_TimeoutReply,
#endif
} MessageID_t;

#ifdef CONFIG_KERNEL_MCS
#define MAX_MSG_SIZE MAX(n_syscallMessage, MAX(n_timeoutMessage, n_exceptionMessage))
#else
#define MAX_MSG_SIZE MAX(n_syscallMessage, n_exceptionMessage)
#endif
extern const regoff_t fault_messages[][MAX_MSG_SIZE] VISIBLE;

static inline void setRegister(tcb_t *thread, regoff_t reg, register_t w)
{
    thread->tcbArch.tcbContext.registers[reg] = w;
}

static inline register_t PURE getRegister(tcb_t *thread, regoff_t reg)
{
    return thread->tcbArch.tcbContext.registers[reg];
}

#ifdef CONFIG_KERNEL_MCS
word_t getNBSendRecvDest(void);
#endif

