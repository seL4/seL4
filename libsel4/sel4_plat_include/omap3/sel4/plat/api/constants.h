/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#pragma once

#ifdef HAVE_AUTOCONF
#include <autoconf.h>
#endif

/* Cortex a8 manual, table 12-11 */
#define seL4_NumHWBreakpoints (8)
#define seL4_NumExclusiveBreakpoints (6)
#define seL4_NumExclusiveWatchpoints (2)
#ifdef CONFIG_HARDWARE_DEBUG_API
#define seL4_FirstWatchpoint (6)
#define seL4_NumDualFunctionMonitors (0)
#endif

/* First address in the virtual address space that is not accessible to user level */
#define seL4_UserTop 0xf0000000

