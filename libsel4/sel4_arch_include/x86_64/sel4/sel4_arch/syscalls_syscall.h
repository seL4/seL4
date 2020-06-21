/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#pragma once

#include <autoconf.h>
#include <sel4/functions.h>
#include <sel4/types.h>

#ifdef CONFIG_KERNEL_MCS
#define MCS_REPLY_DECL register seL4_Word reply_reg asm("r12") = reply
#define MCS_REPLY ,"r"(reply_reg)
#else
#define MCS_REPLY_DECL
#define MCS_REPLY
#endif

static inline void x64_sys_send(seL4_Word sys, seL4_Word dest, seL4_Word info, seL4_Word msg0, seL4_Word msg1,
                                seL4_Word msg2, seL4_Word msg3)
{
    register seL4_Word mr0 asm("r10") = msg0;
    register seL4_Word mr1 asm("r8") = msg1;
    register seL4_Word mr2 asm("r9") = msg2;
    register seL4_Word mr3 asm("r15") = msg3;

    asm volatile(
        "movq   %%rsp, %%rbx        \n"
        "syscall                    \n"
        "movq   %%rbx, %%rsp        \n"
        :
        : "d"(sys),
        "D"(dest),
        "S"(info),
        "r"(mr0),
        "r"(mr1),
        "r"(mr2),
        "r"(mr3)
        : "%rbx", "%rcx", "%r11"
    );
}

#ifndef CONFIG_KERNEL_MCS
static inline void x64_sys_reply(seL4_Word sys, seL4_Word info, seL4_Word msg0, seL4_Word msg1, seL4_Word msg2,
                                 seL4_Word msg3)
{
    register seL4_Word mr0 asm("r10") = msg0;
    register seL4_Word mr1 asm("r8") = msg1;
    register seL4_Word mr2 asm("r9") = msg2;
    register seL4_Word mr3 asm("r15") = msg3;

    asm volatile(
        "movq   %%rsp, %%rbx        \n"
        "syscall                    \n"
        "movq   %%rbx, %%rsp        \n"
        :
        : "d"(sys),
        "S"(info),
        "r"(mr0),
        "r"(mr1),
        "r"(mr2),
        "r"(mr3)
        : "%rbx", "%rcx", "%r11"
    );
}
#endif /* CONFIG_KERNEL_MCS */

static inline void x64_sys_send_null(seL4_Word sys, seL4_Word dest, seL4_Word info)
{
    asm volatile(
        "movq   %%rsp, %%rbx        \n"
        "syscall                    \n"
        "movq   %%rbx, %%rsp        \n"
        :
        : "d"(sys),
        "D"(dest),
        "S"(info)
        : "%rcx", "%rbx", "%r11"
    );
}

static inline void x64_sys_recv(seL4_Word sys, seL4_Word src, seL4_Word *out_badge, seL4_Word *out_info,
                                seL4_Word *out_mr0, seL4_Word *out_mr1, seL4_Word *out_mr2, seL4_Word *out_mr3,
                                LIBSEL4_UNUSED seL4_Word reply)
{
    register seL4_Word mr0 asm("r10");
    register seL4_Word mr1 asm("r8");
    register seL4_Word mr2 asm("r9");
    register seL4_Word mr3 asm("r15");
    MCS_REPLY_DECL;

    asm volatile(
        "movq   %%rsp, %%rbx    \n"
        "syscall                \n"
        "movq   %%rbx, %%rsp    \n"
        : "=D"(*out_badge),
        "=S"(*out_info),
        "=r"(mr0),
        "=r"(mr1),
        "=r"(mr2),
        "=r"(mr3)
        : "d"(sys),
        "D"(src)
        MCS_REPLY
        : "%rcx", "%rbx", "r11", "memory"
    );
    *out_mr0 = mr0;
    *out_mr1 = mr1;
    *out_mr2 = mr2;
    *out_mr3 = mr3;
}

static inline void x64_sys_send_recv(seL4_Word sys, seL4_Word dest, seL4_Word *out_dest, seL4_Word info,
                                     seL4_Word *out_info, seL4_Word *in_out_mr0, seL4_Word *in_out_mr1, seL4_Word *in_out_mr2, seL4_Word *in_out_mr3,
                                     LIBSEL4_UNUSED seL4_Word reply)
{
    register seL4_Word mr0 asm("r10") = *in_out_mr0;
    register seL4_Word mr1 asm("r8") = *in_out_mr1;
    register seL4_Word mr2 asm("r9") = *in_out_mr2;
    register seL4_Word mr3 asm("r15") = *in_out_mr3;
    MCS_REPLY_DECL;

    asm volatile(
        "movq   %%rsp, %%rbx    \n"
        "syscall                \n"
        "movq   %%rbx, %%rsp    \n"
        : "=S"(*out_info),
        "=r"(mr0),
        "=r"(mr1),
        "=r"(mr2),
        "=r"(mr3),
        "=D"(*out_dest)
        : "d"(sys),
        "D"(dest),
        "S"(info),
        "r"(mr0),
        "r"(mr1),
        "r"(mr2),
        "r"(mr3)
        MCS_REPLY
        : "%rcx", "%rbx", "r11", "memory"
    );
    *in_out_mr0 = mr0;
    *in_out_mr1 = mr1;
    *in_out_mr2 = mr2;
    *in_out_mr3 = mr3;
}

#ifdef CONFIG_KERNEL_MCS
static inline void x64_sys_nbsend_recv(seL4_Word sys, seL4_Word dest, seL4_Word src, seL4_Word *out_dest,
                                       seL4_Word info, seL4_Word *out_info, seL4_Word *in_out_mr0, seL4_Word *in_out_mr1, seL4_Word *in_out_mr2,
                                       seL4_Word *in_out_mr3, seL4_Word reply)
{
    register seL4_Word mr0 asm("r10") = *in_out_mr0;
    register seL4_Word mr1 asm("r8") = *in_out_mr1;
    register seL4_Word mr2 asm("r9") = *in_out_mr2;
    register seL4_Word mr3 asm("r15") = *in_out_mr3;
    register seL4_Word reply_reg asm("r12") = reply;
    register seL4_Word dest_reg asm("r13") = dest;

    asm volatile(
        "movq   %%rsp, %%rbx    \n"
        "syscall                \n"
        "movq   %%rbx, %%rsp    \n"
        : "=S"(*out_info),
        "=r"(mr0),
        "=r"(mr1),
        "=r"(mr2),
        "=r"(mr3),
        "=D"(*out_dest)
        : "d"(sys),
        "D"(src),
        "S"(info),
        "r"(mr0),
        "r"(mr1),
        "r"(mr2),
        "r"(mr3),
        "r"(reply_reg),
        "r"(dest_reg)
        : "%rcx", "%rbx", "r11", "memory"
    );
    *in_out_mr0 = mr0;
    *in_out_mr1 = mr1;
    *in_out_mr2 = mr2;
    *in_out_mr3 = mr3;
}
#endif /* CONFIG_KERNEL_MCS */

static inline void x64_sys_null(seL4_Word sys)
{
    asm volatile(
        "movq   %%rsp, %%rbx    \n"
        "syscall                \n"
        "movq   %%rbx, %%rsp    \n"
        :
        : "d"(sys)
        : "%rbx", "%rcx", "%rsi", "%rdi", "%r11"
    );
}
