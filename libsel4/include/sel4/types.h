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

#ifndef __LIBSEL4_TYPES_H
#define __LIBSEL4_TYPES_H

#ifdef HAVE_AUTOCONF
#include <autoconf.h>
#endif
#include <sel4/simple_types.h>
#include <sel4/macros.h>
#include <sel4/arch/types.h>
#include <sel4/sel4_arch/types.h>
#include <sel4/sel4_arch/types_gen.h>
#include <sel4/syscall.h>
#include <sel4/objecttype.h>
#include <sel4/sel4_arch/objecttype.h>
#include <sel4/arch/objecttype.h>
#include <sel4/errors.h>
#include <sel4/constants.h>
#include <sel4/shared_types_gen.h>
#include <sel4/shared_types.h>
#include <sel4/mode/types.h>

#define seL4_UntypedRetypeMaxObjects 256

typedef seL4_CPtr seL4_CNode;
typedef seL4_CPtr seL4_IRQHandler;
typedef seL4_CPtr seL4_IRQControl;
typedef seL4_CPtr seL4_TCB;
typedef seL4_CPtr seL4_Untyped;
typedef seL4_CPtr seL4_DomainSet;

#define seL4_NilData 0

#include <sel4/arch/constants.h>

#endif
