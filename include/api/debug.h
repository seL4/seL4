/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#pragma once

#include <config.h>

#ifdef CONFIG_PRINTING

#include <types.h>

void print_thread_fault(tcb_t *tptr);

#ifdef CONFIG_DEBUG_BUILD

void debug_printKernelEntryReason(void);
/* Prints the user context and stack trace of the current thread */
void debug_printUserState(void);
void debug_printTCB(tcb_t *tcb);
void debug_dumpScheduler(void);

#endif /* CONFIG_DEBUG_BUILD */

#endif /* CONFIG_PRINTING */
