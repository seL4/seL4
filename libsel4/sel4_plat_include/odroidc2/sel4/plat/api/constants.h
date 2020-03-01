/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#ifndef __LIBSEL4_SEL4_PLAT_API_CONSTANTS_H_
#define __LIBSEL4_SEL4_PLAT_API_CONSTANTS_H_

#ifdef HAVE_AUTOCONF
#include <autoconf.h>
#endif

/* Cortex A53 manual, section 11.6.1 */
#define seL4_NumHWBreakpoints (10)
#define seL4_NumExclusiveBreakpoints (6)
#define seL4_NumExclusiveWatchpoints (4)
#ifdef CONFIG_HARDWARE_DEBUG_API
#define seL4_FirstWatchpoint (6)
#define seL4_NumDualFunctionMonitors (0)
#endif

#endif /* __LIBSEL4_SEL4_PLAT_API_CONSTANTS_H_ */
