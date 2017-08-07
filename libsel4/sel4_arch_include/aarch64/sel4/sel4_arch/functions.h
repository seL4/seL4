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

#ifndef __LIBSEL4_SEL4_SEL4_ARCH_FUNCTIONS_H_
#define __LIBSEL4_SEL4_SEL4_ARCH_FUNCTIONS_H_

#include <autoconf.h>
#include <sel4/constants.h>

LIBSEL4_INLINE_FUNC seL4_IPCBuffer*
seL4_GetIPCBuffer(void)
{
    seL4_Word reg;
    asm ("mrs %0, tpidrro_el0" : "=r" (reg));
    return (seL4_IPCBuffer*)reg;
}

#endif /* __LIBSEL4_SEL4_SEL4_ARCH_FUNCTIONS_H_ */
