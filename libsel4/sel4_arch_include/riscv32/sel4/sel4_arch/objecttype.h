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

#ifndef __SEL4_ARCH_OBJECT_TYPE_H
#define __SEL4_ARCH_OBJECT_TYPE_H

#ifdef HAVE_AUTOCONF
#include <autoconf.h>
#endif /* HAVE_AUTOCONF */

typedef enum _mode_object {
    seL4_ModeObjectTypeCount = seL4_NonArchObjectTypeCount
} seL4_ModeObjectType;

#endif
