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
#define MCS_PARAM_DECL(r)    register seL4_Word reply_reg asm(r) = reply;
#define MCS_PARAM    , "r"(reply_reg)
#else
#define MCS_PARAM_DECL(r)
#define MCS_PARAM
#endif


static inline void x64_sys_send(seL4_Word sys, seL4_Word dest, seL4_Word info, seL4_Word msg0, seL4_Word msg1,
                                seL4_Word msg2, seL4_Word msg3)
{
    register seL4_Word mr0 asm("r10") = msg0;
    register seL4_Word mr1 asm("r8") = msg1;
    register seL4_Word mr2 asm("r9") = msg2;
    register seL4_Word mr3 asm("r15") = msg3;

    asm volatile(
        "movq   %%rsp, %%rcx        \n"
        "leaq   1f, %%rdx           \n"
        "1:                         \n"
        "sysenter                   \n"
        :
        : "a"(sys),
        "D"(dest),
        "S"(info),
        "r"(mr0),
        "r"(mr1),
        "r"(mr2),
        "r"(mr3)
        : "%rcx", "%rdx"
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
        "movq   %%rsp, %%rcx        \n"
        "leaq   1f, %%rdx           \n"
        "1:                         \n"
        "sysenter                   \n"
        :
        : "a"(sys),
        "S"(info),
        "r"(mr0),
        "r"(mr1),
        "r"(mr2),
        "r"(mr3)
        : "%rdx", "%rcx"
    );
}
#endif

static inline void x64_sys_send_null(seL4_Word sys, seL4_Word dest, seL4_Word info)
{
    asm volatile(
        "movq   %%rsp, %%rcx        \n"
        "leaq   1f, %%rdx           \n"
        "1:                         \n"
        "sysenter                   \n"
        :
        : "a"(sys),
        "D"(dest),
        "S"(info)
        : "%rcx", "%rdx"
    );
}

static inline void x64_sys_recv(seL4_Word sys, seL4_Word src, seL4_Word *out_badge, seL4_Word *out_info,
                                seL4_Word *out_mr0, seL4_Word *out_mr1, seL4_Word *out_mr2, seL4_Word *out_mr3, LIBSEL4_UNUSED seL4_Word reply)
{
    register seL4_Word mr0 asm("r10");
    register seL4_Word mr1 asm("r8");
    register seL4_Word mr2 asm("r9");
    register seL4_Word mr3 asm("r15");
    MCS_PARAM_DECL("r12");

    asm volatile(
        "movq   %%rsp, %%rcx    \n"
        "leaq   1f, %%rdx       \n"
        "1:                     \n"
        "sysenter               \n"
        : "=D"(*out_badge),
        "=S"(*out_info),
        "=r"(mr0),
        "=r"(mr1),
        "=r"(mr2),
        "=r"(mr3)
        : "a"(sys),
        "D"(src),
        MCS_PARAM
        : "%rcx", "%rdx", "memory"
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
    MCS_PARAM_DECL("r12");

    asm volatile(
        "movq   %%rsp, %%rcx    \n"
        "leaq   1f, %%rdx       \n"
        "1:                     \n"
        "sysenter               \n"
        : "=S"(*out_info),
        "=r"(mr0),
        "=r"(mr1),
        "=r"(mr2),
        "=r"(mr3),
        "=D"(*out_dest)
        : "a"(sys),
        "D"(dest),
        "S"(info),
        "r"(mr0),
        "r"(mr1),
        "r"(mr2),
        "r"(mr3),
        MCS_PARAM
        : "%rcx", "%rdx", "memory"
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
        "movq   %%rsp, %%rcx    \n"
        "leaq   1f, %%rdx       \n"
        "1:                     \n"
        "sysenter               \n"
        : "=S"(*out_info),
        "=r"(mr0),
        "=r"(mr1),
        "=D"(*out_dest)
        : "a"(sys),
        "D"(src),
        "S"(info),
        "r"(mr0),
        "r"(mr1),
        "r"(reply_reg),
        "r"(dest_reg)
        : "%rcx", "%rdx", "memory"
    );

    *in_out_mr0 = mr0;
    *in_out_mr1 = mr1;
    *in_out_mr2 = mr2;
    *in_out_mr3 = mr3;
}
#endif

static inline void x64_sys_null(seL4_Word sys)
{
    asm volatile(
        "movq   %%rsp, %%rcx    \n"
        "leaq   1f, %%rdx       \n"
        "1:                     \n"
        "sysenter               \n"
        :
        : "a"(sys)
        : "%rbx", "%rcx", "%rdx", "%rsi", "%rdi", "memory"
    );
}

