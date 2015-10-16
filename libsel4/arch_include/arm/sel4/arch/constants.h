/*
 * Copyright 2014, NICTA
 *
 * This software may be distributed and modified according to the terms of
 * the BSD 2-Clause license. Note that NO WARRANTY is provided.
 * See "LICENSE_BSD2.txt" for details.
 *
 * @TAG(NICTA_BSD)
 */

#ifndef __LIBSEL4_ARCH_CONSTANTS_H
#define __LIBSEL4_ARCH_CONSTANTS_H

#include <sel4/arch/objecttype.h>

#if CONFIG_MAX_NUM_TRACE_POINTS > 0
/* size of kernel log buffer in bytes */
#define seL4_LogBufferSize (1<<20)
#endif /* CONFIG_MAX_NUM_TRACE_POINTS > 0 */

#endif
