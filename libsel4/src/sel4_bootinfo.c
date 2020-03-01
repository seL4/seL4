/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#include <sel4/sel4.h>

/** Userland per-thread IPC buffer address **/
__thread seL4_IPCBuffer *__sel4_ipc_buffer;

/** Consider moving bootinfo into libsel4_startup */
seL4_BootInfo *bootinfo;

/** Consider moving seL4_InitBootInfo into libsel4_startup */
void seL4_InitBootInfo(seL4_BootInfo *bi)
{
    bootinfo = bi;
}

seL4_BootInfo *seL4_GetBootInfo()
{
    return bootinfo;
}
