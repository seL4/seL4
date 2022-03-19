/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#pragma once

#include <autoconf.h>

#if !defined(CONFIG_ARM_CORTEX_A57)
#error CONFIG_ARM_CORTEX_A57 is not defined
#endif

/* Cortex-A57 Manual, Section 10.6.1 */
#define seL4_NumHWBreakpoints           10
#define seL4_NumExclusiveBreakpoints     6
#define seL4_NumExclusiveWatchpoints     4

#ifdef CONFIG_HARDWARE_DEBUG_API

#define seL4_FirstBreakpoint             0
#define seL4_FirstWatchpoint             6

#define seL4_NumDualFunctionMonitors     0
#define seL4_FirstDualFunctionMonitor    (-1)

#endif /* CONFIG_HARDWARE_DEBUG_API */
