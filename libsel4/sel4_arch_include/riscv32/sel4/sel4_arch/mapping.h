/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#pragma once

#include <autoconf.h>

#include <sel4/sel4_arch/mapping.h>

#define SEL4_MAPPING_LOOKUP_LEVEL 2

LIBSEL4_INLINE_FUNC seL4_Word seL4_MappingFailedLookupLevel()
{
    return seL4_GetMR(SEL4_MAPPING_LOOKUP_LEVEL);
}

#if CONFIG_PT_LEVELS == 2
#define SEL4_MAPPING_LOOKUP_NO_PT 22
#else
#error "Only 2-level PTs are supported for RV32"
#endif
