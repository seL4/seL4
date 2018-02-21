/*
 * Copyright 2018, Data61
 * Commonwealth Scientific and Industrial Research Organisation (CSIRO)
 * ABN 41 687 119 230.
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(DATA61_GPL)
 */

/*
 *
 * Copyright 2016, 2017 Hesham Almatary, Data61/CSIRO <hesham.almatary@data61.csiro.au>
 * Copyright 2015, 2016 Hesham Almatary <heshamelmatary@gmail.com>
 */

#ifndef __LIBSEL4_ARCH_FUNCTIONS_H
#define __LIBSEL4_ARCH_FUNCTIONS_H

#include <sel4/types.h>

static inline seL4_IPCBuffer*
seL4_GetIPCBuffer(void)
{
    seL4_Word reg;
    asm ("mv %0, tp" : "=r"(reg));
    return (seL4_IPCBuffer*)reg;
}


LIBSEL4_INLINE_FUNC seL4_Word
seL4_GetMR(int i)
{
    return seL4_GetIPCBuffer()->msg[i];
}

LIBSEL4_INLINE_FUNC void
seL4_SetMR(int i, seL4_Word mr)
{
    seL4_GetIPCBuffer()->msg[i] = mr;
}

static inline seL4_Word
seL4_GetUserData(void)
{
    return seL4_GetIPCBuffer()->userData;
}

static inline void
seL4_SetUserData(seL4_Word data)
{
    seL4_GetIPCBuffer()->userData = data;
}

static inline seL4_Word
seL4_GetBadge(int i)
{
    return seL4_GetIPCBuffer()->caps_or_badges[i];
}

static inline seL4_CPtr
seL4_GetCap(int i)
{
    return (seL4_CPtr)seL4_GetIPCBuffer()->caps_or_badges[i];
}

static inline void
seL4_SetCap(int i, seL4_CPtr cptr)
{
    seL4_GetIPCBuffer()->caps_or_badges[i] = (seL4_Word)cptr;
}

static inline void
seL4_GetCapReceivePath(seL4_CPtr* receiveCNode, seL4_CPtr* receiveIndex, seL4_Word* receiveDepth)
{
    seL4_IPCBuffer* ipcbuffer = seL4_GetIPCBuffer();
    if (receiveCNode != (void*)0) {
        *receiveCNode = ipcbuffer->receiveCNode;
    }

    if (receiveIndex != (void*)0) {
        *receiveIndex = ipcbuffer->receiveIndex;
    }

    if (receiveDepth != (void*)0) {
        *receiveDepth = ipcbuffer->receiveDepth;
    }
}

static inline void
seL4_SetCapReceivePath(seL4_CPtr receiveCNode, seL4_CPtr receiveIndex, seL4_Word receiveDepth)
{
    seL4_IPCBuffer* ipcbuffer = seL4_GetIPCBuffer();
    ipcbuffer->receiveCNode = receiveCNode;
    ipcbuffer->receiveIndex = receiveIndex;
    ipcbuffer->receiveDepth = receiveDepth;
}
#endif
