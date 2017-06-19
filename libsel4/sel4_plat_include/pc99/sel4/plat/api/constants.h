/*
 * Copyright 2017, Data61
 * Commonwealth Scientific and Industrial Research Organisation (CSIRO)
 * ABN 41 687 119 230.
 *
 * This software may be distributed and modified according to the terms of
 * the BSD 2-Clause license. Note that NO WARRANTY is provided.
 * See "LICENSE_BSD2.txt" for details.
 *
 * @TAG(DATA61_BSD)
 */

#ifndef __LIBSEL4_SEL4_PLAT_API_CONSTANTS_H_
#define __LIBSEL4_SEL4_PLAT_API_CONSTANTS_H_

#ifdef HAVE_AUTOCONF
#include <autoconf.h>
#endif

/* Defined for each architecture: the number of hardware breakpoints
 * available.
 */
#ifdef CONFIG_HARDWARE_DEBUG_API
#define seL4_NumHWBreakpoints (4)
#define seL4_FirstBreakpoint (-1)
#define seL4_NumExclusiveBreakpoints (0)
#define seL4_FirstWatchpoint (-1)
#define seL4_NumExclusiveWatchpoints (0)
#define seL4_FirstDualFunctionMonitor (0)
#define seL4_NumDualFunctionMonitors (4)
#endif

#endif /* __LIBSEL4_SEL4_PLAT_API_CONSTANTS_H_ */
