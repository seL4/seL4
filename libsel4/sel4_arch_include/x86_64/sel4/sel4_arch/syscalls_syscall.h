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

#ifndef __LIBSEL4_SEL4_SEL4_ARCH_SYSCALLS_SYSCALL_H_
#define __LIBSEL4_SEL4_SEL4_ARCH_SYSCALLS_SYSCALL_H_

#include <autoconf.h>
#include <sel4/arch/functions.h>
#include <sel4/types.h>

static inline void
x64_sys_send(seL4_Word sys, seL4_Word dest, seL4_Word info, seL4_Word msg0, seL4_Word msg1, seL4_Word msg2, seL4_Word msg3)
{
    register seL4_Word mr0 asm("r10") = msg0;
    register seL4_Word mr1 asm("r8") = msg1;
    register seL4_Word mr2 asm("r9") = msg2;
    register seL4_Word mr3 asm("r15") = msg3;

    asm volatile (
        "movq   %%rsp, %%rbx        \n"
        "syscall                    \n"
        "movq   %%rbx, %%rsp        \n"
        :
        : "d" (sys),
        "D" (dest),
        "S" (info),
        "r" (mr0),
        "r" (mr1),
        "r" (mr2),
        "r" (mr3)
        : "%rcx", "%rbx", "r11"
    );
}

static inline void
x64_sys_reply(seL4_Word sys, seL4_Word info, seL4_Word msg0, seL4_Word msg1, seL4_Word msg2, seL4_Word msg3)
{
    register seL4_Word mr0 asm("r10") = msg0;
    register seL4_Word mr1 asm("r8") = msg1;
    register seL4_Word mr2 asm("r9") = msg2;
    register seL4_Word mr3 asm("r15") = msg3;

    asm volatile (
        "movq   %%rsp, %%rbx        \n"
        "syscall                    \n"
        "movq   %%rbx, %%rsp        \n"
        :
        : "d" (sys),
        "S" (info),
        "r" (mr0),
        "r" (mr1),
        "r" (mr2),
        "r" (mr3)
        : "%rbx", "%rcx", "%r11"
    );
}

static inline void
x64_sys_send_null(seL4_Word sys, seL4_Word dest, seL4_Word info)
{
    asm volatile (
        "movq   %%rsp, %%rbx        \n"
        "syscall                    \n"
        "movq   %%rbx, %%rsp        \n"
        :
        : "d" (sys),
        "D" (dest),
        "S" (info)
        : "%rcx", "%rbx", "%r11"
    );
}

static inline void
x64_sys_recv(seL4_Word sys, seL4_Word src, seL4_Word *out_badge, seL4_Word *out_info, seL4_Word *out_mr0, seL4_Word *out_mr1, seL4_Word *out_mr2, seL4_Word *out_mr3)
{
    register seL4_Word mr0 asm("r10");
    register seL4_Word mr1 asm("r8");
    register seL4_Word mr2 asm("r9");
    register seL4_Word mr3 asm("r15");

    asm volatile (
        "movq   %%rsp, %%rbx    \n"
        "syscall                \n"
        "movq   %%rbx, %%rsp    \n"
        : "=D" (*out_badge),
        "=S" (*out_info),
        "=r" (mr0),
        "=r" (mr1),
        "=r" (mr2),
        "=r" (mr3)
        : "d" (sys),
        "D" (src)
        : "%rcx", "%rbx", "r11", "memory"
    );
    *out_mr0 = mr0;
    *out_mr1 = mr1;
    *out_mr2 = mr2;
    *out_mr3 = mr3;
}

static inline void
x64_sys_send_recv(seL4_Word sys, seL4_Word dest, seL4_Word *out_dest, seL4_Word info, seL4_Word *out_info, seL4_Word *in_out_mr0, seL4_Word *in_out_mr1, seL4_Word *in_out_mr2, seL4_Word *in_out_mr3)
{
    register seL4_Word mr0 asm("r10") = *in_out_mr0;
    register seL4_Word mr1 asm("r8") = *in_out_mr1;
    register seL4_Word mr2 asm("r9") = *in_out_mr2;
    register seL4_Word mr3 asm("r15") = *in_out_mr3;

    asm volatile (
        "movq   %%rsp, %%rbx    \n"
        "syscall                \n"
        "movq   %%rbx, %%rsp    \n"
        : "=S" (*out_info),
        "=r" (mr0),
        "=r" (mr1),
        "=r" (mr2),
        "=r" (mr3),
        "=D" (*out_dest)
        : "d" (sys),
        "D" (dest),
        "S" (info),
        "r" (mr0),
        "r" (mr1),
        "r" (mr2),
        "r" (mr3)
        : "%rcx", "%rbx", "r11", "memory"
    );
    *in_out_mr0 = mr0;
    *in_out_mr1 = mr1;
    *in_out_mr2 = mr2;
    *in_out_mr3 = mr3;
}

static inline void
x64_sys_null(seL4_Word sys)
{
    asm volatile (
        "movq   %%rsp, %%rbx    \n"
        "syscall                \n"
        "movq   %%rbx, %%rsp    \n"
        :
        : "d" (sys)
        : "%rbx", "%rcx", "%rsi", "%rdi", "%r11"
    );
}

#endif /* __LIBSEL4_SEL4_SEL4_ARCH_SYSCALLS_SYSCALL_H_ */
