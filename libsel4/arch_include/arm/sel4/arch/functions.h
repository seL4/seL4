/*
 * Copyright 2014, NICTA
 *
 * This software may be distributed and modified according to the terms of
 * the BSD 2-Clause license. Note that NO WARRANTY is provided.
 * See "LICENSE_BSD2.txt" for details.
 *
 * @TAG(NICTA_BSD)
 */

#ifndef __LIBSEL4_ARCH_FUNCTIONS_H
#define __LIBSEL4_ARCH_FUNCTIONS_H

#include <sel4/types.h>

enum {
    seL4_GlobalsFrame = 0xffffc000,
};

static inline seL4_IPCBuffer*
seL4_GetIPCBuffer(void)
{
    return *(seL4_IPCBuffer**)seL4_GlobalsFrame;
}

static inline seL4_MessageInfo_t
seL4_GetTag(void)
{
    return seL4_GetIPCBuffer()->tag;
}

static inline void
seL4_SetTag(seL4_MessageInfo_t tag)
{
    seL4_GetIPCBuffer()->tag = tag;
}

static inline seL4_Word
seL4_GetMR(int i)
{
    return seL4_GetIPCBuffer()->msg[i];
}

static inline void
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

static inline seL4_CapData_t
seL4_GetBadge(int i)
{
    return (seL4_CapData_t) {
        {
            seL4_GetIPCBuffer()->caps_or_badges[i]
        }
    };
}

static inline void
seL4_SetCap(int i, seL4_CPtr cptr)
{
    seL4_GetIPCBuffer()->caps_or_badges[i] = (seL4_Word)cptr;
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
