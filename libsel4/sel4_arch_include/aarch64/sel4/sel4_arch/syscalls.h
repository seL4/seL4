/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 * Copyright 2024, Capabilities Limited
 * CHERI support contributed by Capabilities Limited was developed by Hesham Almatary
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#pragma once

#include <sel4/config.h>
#include <sel4/functions.h>
#include <sel4/types.h>

#define STR_HELPER(x) #x
#define STR(x) STR_HELPER(x)

#if defined(CONFIG_HAVE_CHERI)
#define REG(n) "c" STR(n)
#define REGN(n) "c" STR(n)
#define ASM_MR_CONSTR "C"
#else
#define REG(n) "x" STR(n)
#define REGN(n) STR(n)
#define ASM_MR_CONSTR "r"
#endif

#ifdef CONFIG_KERNEL_MCS
#define MCS_PARAM_DECL(r)    register seL4_Register reply_reg asm(r) = reply
#define MCS_PARAM    , ASM_MR_CONSTR(reply_reg)
#else
#define MCS_PARAM_DECL(r)
#define MCS_PARAM
#endif

/*
 * To simplify the definition of the various seL4 syscalls/syscall-wrappers we define
 * some helper assembly functions. These functions are designed to cover the different
 * cases of sending/receiving data in registers to/from the kernel. The most 'complex'
 * version is arm_sys_send_recv, and all other functions are limited versions that allow
 * for registers to not be unnecessarily clobbered
 *
 * arm_sys_send: Fills all registers into the kernel, expects nothing to be sent back
 *      by the kernel. Used for direction one way sends that contain data (e.g. seL4_Send,
 *      seL4_NBSend)
 *
 * arm_sys_send_null: Only fills metadata registers into the kernel (skips message
 *      registers). Expects nothing to be sent back by the kernel. Used by directional
 *      one way sends that do not contain data (e.g. seL4_Notify)
 *
 * arm_sys_reply: Similar to arm_sys_send except it does not take a word for the
 *      destination register. Used for undirected one way sends that contain data
 *      (e.g. seL4_Reply)
 *
 * arm_sys_recv: Sends one register (destination) to the kernel and expects all
 *      registers to be returned by the kernel. Used for directed receives that return
 *      data (e.g. seL4_Recv)
 *
 * arm_sys_send_recv: Fills all registers into the kernel and expects all of them
 *      to be filled on return by the kernel. Used for directed send+receives
 *      where data flows both directions (e.g. seL4_Call, seL4_ReplyWait)
 *
 * arm_sys_nbsend_recv: Fills all registers into the kernel and expects all of them
 *      to be filled on return by the kernel. Used for directed send+receives
 *      where data flows both directions on separate caps (e.g. seL4_NBSendRecv)
 *
 * arm_sys_null: Does not send any registers to the kernel or expect anything to
 *      be returned from the kernel. Used to trigger implicit kernel actions without
 *      any data (e.g. seL4_Yield)
 */

static inline void arm_sys_send(seL4_Word sys, seL4_Word dest, seL4_Word info_arg, seL4_Register mr0, seL4_Register mr1,
                                seL4_Register mr2, seL4_Register mr3)
{
    register seL4_Word destptr asm("x0") = dest;
    register seL4_Word info asm("x1") = info_arg;

    /* Load beginning of the message into registers. */
    register seL4_Register msg0 asm(REG(2)) = mr0;
    register seL4_Register msg1 asm(REG(3)) = mr1;
    register seL4_Register msg2 asm(REG(4)) = mr2;
    register seL4_Register msg3 asm(REG(5)) = mr3;

    /* Perform the system call. */
    register seL4_Word scno asm("x7") = sys;
    asm volatile(
        "svc #0"
        : "+r"(destptr), "+" ASM_MR_CONSTR(msg0), "+" ASM_MR_CONSTR(msg1), "+" ASM_MR_CONSTR(msg2),
        "+" ASM_MR_CONSTR(msg3), "+r"(info)
        : "r"(scno)
    );
}

#ifndef CONFIG_KERNEL_MCS
static inline void arm_sys_reply(seL4_Word sys, seL4_Word info_arg, seL4_Register mr0, seL4_Register mr1, seL4_Register mr2,
                                 seL4_Register mr3)
{
    register seL4_Word info asm("x1") = info_arg;

    /* Load beginning of the message into registers. */
    register seL4_Register msg0 asm(REG(2)) = mr0;
    register seL4_Register msg1 asm(REG(3)) = mr1;
    register seL4_Register msg2 asm(REG(4)) = mr2;
    register seL4_Register msg3 asm(REG(5)) = mr3;

    /* Perform the system call. */
    register seL4_Word scno asm("x7") = sys;
    asm volatile(
        "svc #0"
        : "+" ASM_MR_CONSTR(msg0), "+" ASM_MR_CONSTR(msg1), "+" ASM_MR_CONSTR(msg2), "+" ASM_MR_CONSTR(msg3),
        "+r"(info)
        : "r"(scno)
    );
}
#endif

static inline void arm_sys_send_null(seL4_Word sys, seL4_Word src, seL4_Word info_arg)
{
    register seL4_Word destptr asm("x0") = src;
    register seL4_Word info asm("x1") = info_arg;

    /* Perform the system call. */
    register seL4_Word scno asm("x7") = sys;
    asm volatile(
        "svc #0"
        : "+r"(destptr), "+r"(info)
        : "r"(scno)
    );
}

static inline void arm_sys_recv(seL4_Word sys, seL4_Word src, seL4_Word *out_badge, seL4_Word *out_info,
                                seL4_Register *out_mr0, seL4_Register *out_mr1, seL4_Register *out_mr2, seL4_Register *out_mr3, LIBSEL4_UNUSED seL4_Register reply)
{
    register seL4_Word src_and_badge asm("x0") = src;
    register seL4_Word info asm("x1");

    /* Incoming message registers. */
    register seL4_Register msg0 asm(REG(2));
    register seL4_Register msg1 asm(REG(3));
    register seL4_Register msg2 asm(REG(4));
    register seL4_Register msg3 asm(REG(5));
    MCS_PARAM_DECL(REG(6));

    /* Perform the system call. */
    register seL4_Word scno asm("x7") = sys;
    asm volatile(
        "svc #0"
        : "=" ASM_MR_CONSTR(msg0), "=" ASM_MR_CONSTR(msg1), "=" ASM_MR_CONSTR(msg2), "=" ASM_MR_CONSTR(msg3),
        "=r"(info), "+r"(src_and_badge)
        : "r"(scno) MCS_PARAM
        : "memory"
    );
    *out_badge = src_and_badge;
    *out_info = info;
    *out_mr0 = msg0;
    *out_mr1 = msg1;
    *out_mr2 = msg2;
    *out_mr3 = msg3;
}

static inline void arm_sys_send_recv(seL4_Word sys, seL4_Word dest, seL4_Word *out_badge, seL4_Word info_arg,
                                     seL4_Word *out_info, seL4_Register *in_out_mr0, seL4_Register *in_out_mr1, seL4_Register *in_out_mr2, seL4_Register *in_out_mr3,
                                     LIBSEL4_UNUSED seL4_Register reply)
{
    register seL4_Word destptr asm("x0") = dest;
    register seL4_Word info asm("x1") = info_arg;

    /* Load beginning of the message into registers. */
    register seL4_Register msg0 asm(REG(2)) = *in_out_mr0;
    register seL4_Register msg1 asm(REG(3)) = *in_out_mr1;
    register seL4_Register msg2 asm(REG(4)) = *in_out_mr2;
    register seL4_Register msg3 asm(REG(5)) = *in_out_mr3;
    MCS_PARAM_DECL(REG(6));

    /* Perform the system call. */
    register seL4_Word scno asm("x7") = sys;
    asm volatile(
        "svc #0"
        : "+" ASM_MR_CONSTR(msg0), "+" ASM_MR_CONSTR(msg1), "+" ASM_MR_CONSTR(msg2), "+" ASM_MR_CONSTR(msg3),
        "+r"(info), "+r"(destptr)
        : "r"(scno) MCS_PARAM
        : "memory"
    );
    *out_info = info;
    *out_badge = destptr;
    *in_out_mr0 = msg0;
    *in_out_mr1 = msg1;
    *in_out_mr2 = msg2;
    *in_out_mr3 = msg3;
}

#ifdef CONFIG_KERNEL_MCS
static inline void arm_sys_nbsend_recv(seL4_Word sys, seL4_Word dest, seL4_Word src, seL4_Word *out_badge,
                                       seL4_Word info_arg,
                                       seL4_Register *out_info, seL4_Register *in_out_mr0, seL4_Register *in_out_mr1, seL4_Register *in_out_mr2,
                                       seL4_Register *in_out_mr3, seL4_Word reply)
{
    register seL4_Word src_and_badge asm("x0") = src;
    register seL4_Word info asm("x1") = info_arg;

    /* Load the beginning of the message info registers */
    register seL4_Register msg0 asm(REG(2)) = *in_out_mr0;
    register seL4_Register msg1 asm(REG(3)) = *in_out_mr1;
    register seL4_Register msg2 asm(REG(4)) = *in_out_mr2;
    register seL4_Register msg3 asm(REG(5)) = *in_out_mr3;

    register seL4_Word reply_reg asm("x6") = reply;
    register seL4_Word dest_reg asm("x8") = dest;

    /* Perform the system call. */
    register seL4_Word scno asm("x7") = sys;
    asm volatile(
        "svc #0"
        : "+" ASM_MR_CONSTR(msg0), "+" ASM_MR_CONSTR(msg1), "+" ASM_MR_CONSTR(msg2), "+" ASM_MR_CONSTR(msg3),
        "+r"(src_and_badge), "+r"(info)
        : "r"(scno), "r"(reply_reg), "r"(dest_reg)
        : "memory"
    );

    *out_badge = src_and_badge;
    *out_info = info;
    *in_out_mr0 = msg0;
    *in_out_mr1 = msg1;
    *in_out_mr2 = msg2;
    *in_out_mr3 = msg3;
}
#endif

static inline void arm_sys_null(seL4_Word sys)
{
    register seL4_Word scno asm("x7") = sys;
    asm volatile(
        "svc #0"
        : /* no outputs */
        : "r"(scno)
    );
}
