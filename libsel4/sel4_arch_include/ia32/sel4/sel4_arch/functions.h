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

#include <sel4/types.h>
#include <sel4/macros.h>

/* the segment loaded into GS points directly to the IPC buffer */

#define SEL4_GET_IPCBUF_SCALE(field, i, res) \
    do {\
        asm volatile ("movl %%fs:%c2(,%1,%c3), %0"\
                      : [result] "=r" (res) /* outputs */\
                      : [scale] "r" (i), /* inputs */\
                        [offset] "i" (SEL4_OFFSETOF(seL4_IPCBuffer, field)),\
                        [scale_factor] "i" (sizeof(seL4_Word))\
                       /* no clobber */);\
    } while(0)


#define SEL4_SET_IPCBUF_SCALE(field, i, val) \
    do {\
        asm volatile ("movl %0, %%fs:%c2(,%1,%c3)"\
                      : /* no outputs */\
                      : [value] "r" (val), /* inputs */\
                        [scale] "r" (i),\
                        [offset] "i" (SEL4_OFFSETOF(seL4_IPCBuffer, field)),\
                        [scale_factor] "i" (sizeof(seL4_Word))\
                      : "memory"); /* clobber */\
    } while(0)

#define SEL4_GET_IPCBUF(field, res) \
    do {\
        asm volatile ("movl %%fs:%c1, %0"\
                      : [result] "=r" (res) /* inputs */\
                      : [offset] "i" (SEL4_OFFSETOF(seL4_IPCBuffer, field)) /* outputs */\
                       /* no clobber */);\
    } while(0)


#define SEL4_SET_IPCBUF(field, val) \
    do {\
        asm volatile ("movl %0, %%fs:%c1"\
                      : /* no outputs */\
                      : [value] "r" (val), /* inputs */\
                        [offset] "i" (SEL4_OFFSETOF(seL4_IPCBuffer, field))\
                      : "memory"); /* clobber */\
    } while(0)

#endif
