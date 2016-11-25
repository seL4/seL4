/*
 * Copyright 2014, NICTA
 *
 * This software may be distributed and modified according to the terms of
 * the BSD 2-Clause license. Note that NO WARRANTY is provided.
 * See "LICENSE_BSD2.txt" for details.
 *
 * @TAG(NICTA_BSD)
 */

#ifndef __LIBSEL4_SEL4_ARCH_MAPPING
#define __LIBSEL4_SEL4_ARCH_MAPPING

#include <autoconf.h>

#define SEL4_MAPPING_LOOKUP_LEVEL 2
#ifdef CONFIG_PAE_PAGING
#define SEL4_MAPPING_LOOKUP_NO_PT 21
#define SEL4_MAPPING_LOOKUP_NO_PD 30
#else
#define SEL4_MAPPING_LOOKUP_NO_PT 22
#endif

LIBSEL4_INLINE_FUNC seL4_Word seL4_MappingFailedLookupLevel()
{
    return seL4_GetMR(SEL4_MAPPING_LOOKUP_LEVEL);
}

#endif
