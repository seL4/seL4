/*
 * Copyright 2014, NICTA
 *
 * This software may be distributed and modified according to the terms of
 * the BSD 2-Clause license. Note that NO WARRANTY is provided.
 * See "LICENSE_BSD2.txt" for details.
 *
 * @TAG(NICTA_BSD)
 */

#ifndef __SEL4_ARCH_OBJECT_TYPE_H
#define __SEL4_ARCH_OBJECT_TYPE_H

#ifdef HAVE_AUTOCONF
#include <autoconf.h>
#endif /* HAVE_AUTOCONF */

typedef enum _mode_object {
#ifdef CONFIG_PAE_PAGING
    seL4_IA32_PDPTObject = seL4_NonArchObjectTypeCount,
    seL4_ModeObjectTypeCount
#else
    seL4_ModeObjectTypeCount = seL4_NonArchObjectTypeCount,
#endif
} seL4_ModeObjectType;

#ifndef CONFIG_PAE_PAGING
#define seL4_IA32_PDPTObject 0xffffffff
#endif

#endif
