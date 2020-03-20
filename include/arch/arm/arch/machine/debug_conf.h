/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#pragma once

#include <config.h>

/* These are used to force specific outcomes for various combinations of
 * settings for the state of CONFIG_ARM_HYPERVISOR_SUPPORT,
 * CONFIG_ARM_HYP_ENABLE_VCPU_CP14_SAVE_AND_RESTORE and
 * CONFIG_HARDWARE_DEBUG_API.
 */

#ifdef CONFIG_ARM_HYPERVISOR_SUPPORT
#ifdef CONFIG_HARDWARE_DEBUG_API
/* With the debug-API on, the ARM-hyp kernel will enable HDCR CP14-related
 * traps whenever it is running a native thread and not a VCPU thread.
 */
#define ARM_HYP_TRAP_CP14_IN_NATIVE_USER_THREADS
#endif

#ifdef CONFIG_ARM_HYP_ENABLE_VCPU_CP14_SAVE_AND_RESTORE
/* When this is enabled, the ARM-hyp kernel will enable
 * CP14 save and restore whenever it is running a VCPU thread. When it's
 * disabled, the hyp kernel will intercept accesses to the CP14
 * coprocessor and deliver them as fault messages to the VCPU's fault
 * handler.
 */
#define ARM_HYP_CP14_SAVE_AND_RESTORE_VCPU_THREADS
#endif
#endif

#ifdef CONFIG_HARDWARE_DEBUG_API
/* If HARDWARE_DEBUG_API is set, then we must save/retore native threads. */
#define ARM_CP14_SAVE_AND_RESTORE_NATIVE_THREADS
#endif

#if defined(ARM_CP14_SAVE_AND_RESTORE_NATIVE_THREADS) || defined(ARM_HYP_CP14_SAVE_AND_RESTORE_VCPU_THREADS)
#define ARM_BASE_CP14_SAVE_AND_RESTORE
#endif
#if defined(ARM_HYP_TRAP_CP14_IN_NATIVE_USER_THREADS) || defined(ARM_HYP_TRAP_CP14_IN_VCPU_THREADS)
#define ARM_HYP_TRAP_CP14
#endif

