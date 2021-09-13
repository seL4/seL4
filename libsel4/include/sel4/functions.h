/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 * Copyright 2015, 2016 Hesham Almatary <heshamelmatary@gmail.com>
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#pragma once

#include <sel4/types.h>
#include <sel4/syscalls.h>

extern __thread seL4_IPCBuffer *__sel4_ipc_buffer;

#ifdef CONFIG_KERNEL_INVOCATION_REPORT_ERROR_IPC
extern __thread char __sel4_print_error;

LIBSEL4_INLINE_FUNC char *seL4_GetDebugError(void)
{
    return (char *)(__sel4_ipc_buffer->msg + DEBUG_MESSAGE_START);
}

LIBSEL4_INLINE_FUNC void seL4_SetPrintError(char print_error)
{
    __sel4_print_error = print_error;
    return;
}

LIBSEL4_INLINE_FUNC char seL4_CanPrintError(void)
{
    return __sel4_print_error;
}
#endif

LIBSEL4_INLINE_FUNC void seL4_SetIPCBuffer(seL4_IPCBuffer *ipc_buffer)
{
    __sel4_ipc_buffer = ipc_buffer;
    return;
}

LIBSEL4_INLINE_FUNC seL4_IPCBuffer *seL4_GetIPCBuffer(void)
{
    return __sel4_ipc_buffer;
}

LIBSEL4_INLINE_FUNC seL4_Word seL4_GetMR(int i)
{
    return seL4_GetIPCBuffer()->msg[i];
}

LIBSEL4_INLINE_FUNC void seL4_SetMR(int i, seL4_Word mr)
{
    seL4_GetIPCBuffer()->msg[i] = mr;
}

LIBSEL4_INLINE_FUNC seL4_Word seL4_GetUserData(void)
{
    return seL4_GetIPCBuffer()->userData;
}

LIBSEL4_INLINE_FUNC void seL4_SetUserData(seL4_Word data)
{
    seL4_GetIPCBuffer()->userData = data;
}

LIBSEL4_INLINE_FUNC seL4_Word seL4_GetBadge(int i)
{
    return seL4_GetIPCBuffer()->caps_or_badges[i];
}

LIBSEL4_INLINE_FUNC seL4_CPtr seL4_GetCap(int i)
{
    return (seL4_CPtr)seL4_GetIPCBuffer()->caps_or_badges[i];
}

LIBSEL4_INLINE_FUNC void seL4_SetCap(int i, seL4_CPtr cptr)
{
    seL4_GetIPCBuffer()->caps_or_badges[i] = (seL4_Word)cptr;
}

LIBSEL4_INLINE_FUNC void seL4_GetCapReceivePath(seL4_CPtr *receiveCNode, seL4_CPtr *receiveIndex,
                                                seL4_Word *receiveDepth)
{
    seL4_IPCBuffer *ipcbuffer = seL4_GetIPCBuffer();
    if (receiveCNode != (void *)0) {
        *receiveCNode = ipcbuffer->receiveCNode;
    }

    if (receiveIndex != (void *)0) {
        *receiveIndex = ipcbuffer->receiveIndex;
    }

    if (receiveDepth != (void *)0) {
        *receiveDepth = ipcbuffer->receiveDepth;
    }
}

LIBSEL4_INLINE_FUNC void seL4_SetCapReceivePath(seL4_CPtr receiveCNode, seL4_CPtr receiveIndex, seL4_Word receiveDepth)
{
    seL4_IPCBuffer *ipcbuffer = seL4_GetIPCBuffer();
    ipcbuffer->receiveCNode = receiveCNode;
    ipcbuffer->receiveIndex = receiveIndex;
    ipcbuffer->receiveDepth = receiveDepth;
}

