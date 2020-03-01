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

/* ARM1136-JF-S manual, table 13-3 */
#define seL4_NumHWBreakpoints (8)
#define seL4_NumExclusiveBreakpoints (6)
#define seL4_NumExclusiveWatchpoints (2)
#ifdef CONFIG_HARDWARE_DEBUG_API
#define seL4_FirstWatchpoint (6)
#define seL4_NumDualFunctionMonitors (0)
#endif

/* First address in the virtual address space that is not accessible to user level */
#define seL4_UserTop 0xf0000000

#endif /* __LIBSEL4_SEL4_PLAT_API_CONSTANTS_H_ */
