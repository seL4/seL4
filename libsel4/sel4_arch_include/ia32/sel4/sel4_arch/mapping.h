/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#pragma once
#include <autoconf.h>

#define SEL4_MAPPING_LOOKUP_LEVEL 2
#define SEL4_MAPPING_LOOKUP_NO_PT 22

LIBSEL4_INLINE_FUNC seL4_Word seL4_MappingFailedLookupLevel(void)
{
    return seL4_GetMR(SEL4_MAPPING_LOOKUP_LEVEL);
}


