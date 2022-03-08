/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */
#pragma once

#include <config.h>
#include <arch/kernel/vspace.h>

exception_t checkValidIPCBuffer(vptr_t vptr, cap_t cap);
word_t *PURE lookupIPCBuffer(bool_t isReceiver, tcb_t *thread);

exception_t handleVMFault(tcb_t *thread, vm_fault_type_t vm_faultType);

#ifdef CONFIG_KERNEL_LOG_BUFFER
exception_t benchmark_arch_map_logBuffer(word_t frame_cptr);
#endif /* CONFIG_KERNEL_LOG_BUFFER */

#ifdef CONFIG_PRINTING
void Arch_userStackTrace(tcb_t *tptr);
#endif /* CONFIG_PRINTING */
