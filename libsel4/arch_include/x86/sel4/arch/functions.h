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
#include <stddef.h> /* for NULL */

/* the segment loaded into GS points directly to the IPC buffer */

static inline seL4_MessageInfo_t
seL4_GetTag(void)
{
    seL4_MessageInfo_t tag;
    asm volatile ("movl %%gs:0, %0" : "=r"(tag));
    return tag;
}

static inline void
seL4_SetTag(seL4_MessageInfo_t tag)
{
    asm volatile ("movl %0, %%gs:0" :: "r"(tag) : "memory");
}

static inline seL4_Word
seL4_GetMR(int i)
{
    seL4_Word mr;
    asm volatile ("movl %%gs:4(,%1,0x4), %0" : "=r"(mr) : "r"(i));
    return mr;
}

static inline void
seL4_SetMR(int i, seL4_Word mr)
{
    asm volatile ("movl %0, %%gs:4(,%1,0x4)" :: "r"(mr), "r"(i) : "memory");
}

static inline seL4_Word
seL4_GetUserData(void)
{
    seL4_Word data;
    asm volatile ("movl %%gs:484, %0" : "=r"(data));
    return data;
}

static inline void
seL4_SetUserData(seL4_Word data)
{
    asm volatile ("movl %0, %%gs:484" :: "r"(data) : "memory");
}

static inline seL4_CapData_t
seL4_GetBadge(int i)
{
    seL4_CapData_t badge;
    asm volatile ("movl %%gs:488(,%1,0x4), %0" : "=r"(badge) : "r"(i));
    return badge;
}

static inline seL4_CPtr
seL4_GetCap(int i)
{
    seL4_CPtr cptr;
    asm volatile ("movl %%gs:488(,%1,0x4), %0" : "=r"(cptr) : "r"(i));
    return cptr;
}

static inline void
seL4_SetCap(int i, seL4_CPtr cptr)
{
    asm volatile ("movl %0, %%gs:488(,%1,0x4)" :: "r"(cptr), "r"(i) : "memory");
}

static inline void
seL4_GetCapReceivePath(seL4_CPtr* receiveCNode, seL4_CPtr* receiveIndex, seL4_Word* receiveDepth)
{
    if (receiveCNode != NULL) {
        asm volatile ("movl %%gs:500, %0" : "=r"(*receiveCNode));
    }

    if (receiveIndex != NULL) {
        asm volatile ("movl %%gs:504, %0" : "=r"(*receiveIndex));
    }

    if (receiveDepth != NULL) {
        asm volatile ("movl %%gs:508, %0" : "=r"(*receiveDepth));
    }
}

static inline void
seL4_SetCapReceivePath(seL4_CPtr receiveCNode, seL4_CPtr receiveIndex, seL4_Word receiveDepth)
{
    asm volatile ("movl %0, %%gs:500" :: "r"(receiveCNode) : "memory");
    asm volatile ("movl %0, %%gs:504" :: "r"(receiveIndex) : "memory");
    asm volatile ("movl %0, %%gs:508" :: "r"(receiveDepth) : "memory");
}

static inline seL4_IPCBuffer*
seL4_GetIPCBuffer(void)
{
    /* Assume that the address of our IPC buffer is in the user data word. Our
     * parent (or seL4_InitBootInfo in the case of the root task) should have
     * ensured this is true. The user is free to overwrite this data with
     * something else, but should then be aware they lose the functionality of
     * this function.
     */
    return (seL4_IPCBuffer*)seL4_GetUserData();
}

#endif
