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

#define seL4_NumHWBreakpoints (10)
#define seL4_NumExclusiveBreakpoints (6)
#define seL4_NumExclusiveWatchpoints (4)
#ifdef CONFIG_HARDWARE_DEBUG_API
#define seL4_FirstWatchpoint (6)
#define seL4_NumDualFunctionMonitors (0)
#endif

#if CONFIG_WORD_SIZE == 32
/* First address in the virtual address space that is not accessible to user level */
#define seL4_UserTop 0xe0000000
#else
/* otherwise this is defined at the arch level */
#endif

#endif /* __LIBSEL4_SEL4_PLAT_API_CONSTANTS_H_ */
