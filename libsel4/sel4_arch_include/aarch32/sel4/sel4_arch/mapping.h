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

#ifndef __LIBSEL4_SEL4_ARCH_MAPPING
#define __LIBSEL4_SEL4_ARCH_MAPPING

#ifdef HAVE_AUTOCONF
#include <autoconf.h>
#endif

#define SEL4_MAPPING_LOOKUP_LEVEL 2

#ifdef CONFIG_ARM_HYPERVISOR_SUPPORT
#define SEL4_MAPPING_LOOKUP_NO_PT 21
#else
#define SEL4_MAPPING_LOOKUP_NO_PT 20
#endif

LIBSEL4_INLINE_FUNC seL4_Word seL4_MappingFailedLookupLevel(void)
{
    return seL4_GetMR(SEL4_MAPPING_LOOKUP_LEVEL);
}

#endif
