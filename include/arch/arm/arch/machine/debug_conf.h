/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#pragma once

#include <config.h>

/* These are used to force specific outcomes for various combinations of
 * settings for the state of CONFIG_ARM_HYPERVISOR_SUPPORT and
 * CONFIG_HARDWARE_DEBUG_API.
 */

#ifdef CONFIG_ARM_HYPERVISOR_SUPPORT
#ifdef CONFIG_HARDWARE_DEBUG_API
/* With the debug-API on, the ARM-hyp kernel will enable HDCR CP14-related
 * traps whenever it is running a native thread and not a VCPU thread.
 */
#define ARM_HYP_TRAP_CP14_IN_NATIVE_USER_THREADS
#endif
#endif

#ifdef CONFIG_HARDWARE_DEBUG_API
/* If HARDWARE_DEBUG_API is set, then we must save/retore native threads. */
#define ARM_CP14_SAVE_AND_RESTORE_NATIVE_THREADS
#endif

#ifdef ARM_CP14_SAVE_AND_RESTORE_NATIVE_THREADS
#define ARM_BASE_CP14_SAVE_AND_RESTORE
#endif
