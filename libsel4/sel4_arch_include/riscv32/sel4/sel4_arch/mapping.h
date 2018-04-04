/*
 * Copyright 2018, Data61
 * Commonwealth Scientific and Industrial Research Organisation (CSIRO)
 * ABN 41 687 119 230.
 *
 * This software may be distributed and modified according to the terms of
 * the BSD 2-Clause license. Note that NO WARRANTY is provided.
 * See "LICENSE_BSD2.txt" for details.
 *
 * @TAG(DATA61_BSD)
 */

#ifndef __LIBSEL4_SEL4_ARCH_MAPPING
#define __LIBSEL4_SEL4_ARCH_MAPPING

#include <autoconf.h>

#include <sel4/sel4_arch/mapping.h>

#define SEL4_MAPPING_LOOKUP_LEVEL 2

static inline seL4_Word seL4_MappingFailedLookupLevel()
{
    return seL4_GetMR(SEL4_MAPPING_LOOKUP_LEVEL);
}

#if CONFIG_PT_LEVELS == 2
#define SEL4_MAPPING_LOOKUP_NO_PT 22
#else
#error "Only 2-level PTs are supported for RV32"
#endif

#endif
