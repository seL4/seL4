/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 * Copyright 2015, 2016 Hesham Almatary <heshamelmatary@gmail.com>
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#pragma once
#include <sel4/config.h>

#include <sel4/sel4_arch/mapping.h>

#define SEL4_MAPPING_LOOKUP_LEVEL 2

LIBSEL4_INLINE_FUNC seL4_Word seL4_MappingFailedLookupLevel()
{
    return seL4_GetMR(SEL4_MAPPING_LOOKUP_LEVEL);
}

#define SEL4_MAPPING_LOOKUP_NO_PT 21
