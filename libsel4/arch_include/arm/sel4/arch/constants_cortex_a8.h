/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#pragma once

#include <sel4/config.h>

#if !defined(CONFIG_ARM_CORTEX_A8)
#error CONFIG_ARM_CORTEX_A8 is not defined
#endif

/* Cortex-A8 Manual, Table 12-11 */
#define seL4_NumHWBreakpoints           8
#define seL4_NumExclusiveBreakpoints    6
#define seL4_NumExclusiveWatchpoints    2

#ifdef CONFIG_HARDWARE_DEBUG_API

#define seL4_FirstBreakpoint            0
#define seL4_FirstWatchpoint            6

#define seL4_NumDualFunctionMonitors    0
#define seL4_FirstDualFunctionMonitor   (-1)

#endif /* CONFIG_HARDWARE_DEBUG_API */
