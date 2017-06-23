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

#ifndef __LIBSEL4_SEL4_ARCH_FUNCTIONS_H
#define __LIBSEL4_SEL4_ARCH_FUNCTIONS_H

#include <autoconf.h>
#include <sel4/constants.h>
#include <sel4/sel4_arch/constants.h>

LIBSEL4_INLINE_FUNC seL4_IPCBuffer*
seL4_GetIPCBuffer(void)
{
#if defined(CONFIG_IPC_BUF_GLOBALS_FRAME)
    return *(seL4_IPCBuffer**)seL4_GlobalsFrame;
#elif defined(CONFIG_IPC_BUF_TPIDRURW)
    seL4_Word reg;
    asm ("mrc p15, 0, %0, c13, c0, 2" : "=r"(reg));
    return (seL4_IPCBuffer*)reg;
#else
#error "Unknown IPC buffer strateg"
#endif
}

#endif
