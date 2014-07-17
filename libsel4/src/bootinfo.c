/*
 * Copyright 2014, NICTA
 *
 * This software may be distributed and modified according to the terms of
 * the BSD 2-Clause license. Note that NO WARRANTY is provided.
 * See "LICENSE_BSD2.txt" for details.
 *
 * @TAG(NICTA_BSD)
 */

#include <sel4/bootinfo.h>
#include <sel4/arch/functions.h>

seL4_BootInfo* bootinfo;

void seL4_InitBootInfo(seL4_BootInfo* bi)
{
    bootinfo = bi;
    /* Save the address of the IPC buffer for seL4_GetIPCBuffer on IA32. */
    seL4_SetUserData((seL4_Word)bootinfo->ipcBuffer);
}

seL4_BootInfo* seL4_GetBootInfo()
{
    return bootinfo;
}
