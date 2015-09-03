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

/* the segment loaded into GS points directly to the IPC buffer */

static inline seL4_MessageInfo_t
seL4_GetTag(void)
{
    seL4_MessageInfo_t tag;
    asm volatile ("movq %%gs:0, %0" : "=r"(tag));
    return tag;
}

static inline void
seL4_SetTag(seL4_MessageInfo_t tag)
{
    asm volatile ("movq %0, %%gs:0" :: "r"(tag) : "memory");
}

static inline seL4_Word
seL4_GetMR(int i)
{
    seL4_Word mr;
    asm volatile ("movq %%gs:8(,%1,0x8), %0" : "=a"(mr) : "D"(i));
    return mr;
}

static inline void
seL4_SetMR(int i, seL4_Word mr)
{
    asm volatile ("movq %0, %%gs:8(,%1,0x8)" :: "S"(mr), "D"(i) : "memory");
}

static inline seL4_Word
seL4_GetUserData(void)
{
    seL4_Word data;
    asm volatile ("movq %%gs:968, %0" : "=r"(data));
    return data;
}

static inline void
seL4_SetUserData(seL4_Word data)
{
    asm volatile ("movq %0, %%gs:968" :: "r"(data) : "memory");
}

static inline seL4_CapData_t
seL4_GetBadge(int i)
{
    seL4_CapData_t badge;
    asm volatile ("movq %%gs:976(,%1,0x8), %0" : "=r"(badge) : "r"(i));
    return badge;
}

static inline seL4_CPtr
seL4_GetCap(int i)
{
    seL4_CPtr cptr;
    asm volatile ("movq %%gs:976(,%1,0x8), %0" : "=r"(cptr) : "r"(i));
    return cptr;
}

static inline void
seL4_SetCap(int i, seL4_CPtr cptr)
{
    asm volatile ("movq %0, %%gs:976(,%1,0x8)" :: "r"(cptr), "r"(i) : "memory");
}

static inline void
seL4_GetCapReceivePath(seL4_CPtr* receiveCNode, seL4_CPtr* receiveIndex, seL4_Word* receiveDepth)
{
    if (receiveCNode != seL4_Null) {
        asm volatile ("movq %%gs:1000, %0" : "=r"(*receiveCNode));
    }

    if (receiveIndex != seL4_Null) {
        asm volatile ("movq %%gs:1008, %0" : "=r"(*receiveIndex));
    }

    if (receiveDepth != seL4_Null) {
        asm volatile ("movq %%gs:1016, %0" : "=r"(*receiveDepth));
    }
}

static inline void
seL4_SetCapReceivePath(seL4_CPtr receiveCNode, seL4_CPtr receiveIndex, seL4_Word receiveDepth)
{
    asm volatile ("movq %0, %%gs:1000" :: "r"(receiveCNode) : "memory");
    asm volatile ("movq %0, %%gs:1008" :: "r"(receiveIndex) : "memory");
    asm volatile ("movq %0, %%gs:1016" :: "r"(receiveDepth) : "memory");
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
