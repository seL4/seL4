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

#ifndef __LIBSEL4_SEL4_SEL4_ARCH_OBJECTTYPE_H_
#define __LIBSEL4_SEL4_SEL4_ARCH_OBJECTTYPE_H_

#ifdef HAVE_AUTOCONF
#include <autoconf.h>
#endif /* HAVE_AUTOCONF */

typedef enum _mode_object {
    seL4_X86_PDPTObject = seL4_NonArchObjectTypeCount,
    seL4_X64_PML4Object,
#ifdef CONFIG_HUGE_PAGE
    seL4_X64_HugePageObject,
#endif
    seL4_ModeObjectTypeCount
} seL4_seL4ArchObjectType;

/* allow seL4_X86_PDPTObject and seL4_IA32_PDPTObject to be used interchangeable */
#define seL4_IA32_PDPTObject seL4_X86_PDPTObject

#ifndef CONFIG_HUGE_PAGE
#define seL4_X64_HugePageObject 0xfffffffe
#endif

#endif /* __LIBSEL4_SEL4_SEL4_ARCH_OBJECTTYPE_H_ */
