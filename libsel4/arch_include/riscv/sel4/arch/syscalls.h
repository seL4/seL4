/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 * Copyright 2015, 2016 Hesham Almatary <heshamelmatary@gmail.com>
 * Copyright 2024, Capabilities Limited
 * CHERI support contributed by Capabilities Limited was developed by Hesham Almatary
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#pragma once

#include <sel4/config.h>
#include <sel4/functions.h>
#include <sel4/sel4_arch/syscalls.h>
#include <sel4/types.h>

#define STR_HELPER(x) #x
#define STR(x) STR_HELPER(x)

#if defined(CONFIG_HAVE_CHERI)
#define REG(n) "c" STR(n)
#define ASM_ARG_CONSTR "C"
#else
#define REG(n) STR(n)
#define ASM_ARG_CONSTR "r"
#endif

#ifdef CONFIG_KERNEL_MCS
#define MCS_PARAM_DECL(r)    register seL4_Word reply_reg asm(r) = reply
#define MCS_PARAM    , "r"(reply_reg)
#define LIBSEL4_MCS_REPLY reply
#else
#define MCS_PARAM_DECL(r)
#define MCS_PARAM
#define LIBSEL4_MCS_REPLY 0
#endif

static inline void riscv_sys_send(seL4_Word sys, seL4_Word dest, seL4_Word info_arg, seL4_Register mr0, seL4_Register mr1,
                                  seL4_Register mr2, seL4_Register mr3)
{
    register seL4_Word destptr asm("a0") = dest;
    register seL4_Word info asm("a1") = info_arg;

    /* Load beginning of the message into registers. */
    register seL4_Register msg0 asm(REG(a2)) = mr0;
    register seL4_Register msg1 asm(REG(a3)) = mr1;
    register seL4_Register msg2 asm(REG(a4)) = mr2;
    register seL4_Register msg3 asm(REG(a5)) = mr3;

    /* Perform the system call. */
    register seL4_Word scno asm("a7") = sys;
    asm volatile(
        "ecall"
        : "+r"(destptr), "+" ASM_ARG_CONSTR(msg0), "+" ASM_ARG_CONSTR(msg1), "+" ASM_ARG_CONSTR(msg2),
        "+" ASM_ARG_CONSTR(msg3), "+r"(info)
        : "r"(scno)
    );
}

#ifndef CONFIG_KERNEL_MCS
static inline void riscv_sys_reply(seL4_Word sys, seL4_Word info_arg, seL4_Register mr0, seL4_Register mr1, seL4_Register mr2,
                                   seL4_Register mr3)
{
    register seL4_Word info asm("a1") = info_arg;

    /* Load beginning of the message into registers. */
    register seL4_Register msg0 asm(REG(a2)) = mr0;
    register seL4_Register msg1 asm(REG(a3)) = mr1;
    register seL4_Register msg2 asm(REG(a4)) = mr2;
    register seL4_Register msg3 asm(REG(a5)) = mr3;

    /* Perform the system call. */
    register seL4_Word scno asm("a7") = sys;
    asm volatile(
        "ecall"
        : "+" ASM_ARG_CONSTR(msg0), "+" ASM_ARG_CONSTR(msg1), "+" ASM_ARG_CONSTR(msg2), "+" ASM_ARG_CONSTR(msg3),
        "+r"(info)
        : "r"(scno)
    );
}
#endif

static inline void riscv_sys_send_null(seL4_Word sys, seL4_Word src, seL4_Word info_arg)
{
    register seL4_Word destptr asm("a0") = src;
    register seL4_Word info asm("a1") = info_arg;

    /* Perform the system call. */
    register seL4_Word scno asm("a7") = sys;
    asm volatile(
        "ecall"
        : "+r"(destptr), "+r"(info)
        : "r"(scno)
    );
}

static inline void riscv_sys_recv(seL4_Word sys, seL4_Word src, seL4_Word *out_badge, seL4_Word *out_info, seL4_Register
                                  *out_mr0, seL4_Register *out_mr1, seL4_Register *out_mr2, seL4_Register *out_mr3, LIBSEL4_UNUSED seL4_Word reply)
{
    register seL4_Word src_and_badge asm("a0") = src;
    register seL4_Word info asm("a1");

    /* Incoming message registers. */
    register seL4_Register msg0 asm(REG(a2));
    register seL4_Register msg1 asm(REG(a3));
    register seL4_Register msg2 asm(REG(a4));
    register seL4_Register msg3 asm(REG(a5));
    MCS_PARAM_DECL("a6");

    /* Perform the system call. */
    register seL4_Word scno asm("a7") = sys;
    asm volatile(
        "ecall"
        : "=" ASM_ARG_CONSTR(msg0), "=" ASM_ARG_CONSTR(msg1), "=" ASM_ARG_CONSTR(msg2), "=" ASM_ARG_CONSTR(msg3),
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

static inline void riscv_sys_null(seL4_Word sys)
{
    register seL4_Word scno asm("a7") = sys;
    asm volatile(
        "ecall"
        : /* no outputs */
        : "r"(scno)
    );
}

static inline void riscv_sys_send_recv(seL4_Word sys, seL4_Word dest, seL4_Word *out_badge, seL4_Word info_arg,
                                       seL4_Word
                                       *out_info, seL4_Register *in_out_mr0, seL4_Register *in_out_mr1, seL4_Register *in_out_mr2,
                                       seL4_Register
                                       *in_out_mr3, LIBSEL4_UNUSED seL4_Word reply)
{
    register seL4_Word destptr asm("a0") = dest;
    register seL4_Word info asm("a1") = info_arg;

    /* Load beginning of the message into registers. */
    register seL4_Register msg0 asm(REG(a2)) = *in_out_mr0;
    register seL4_Register msg1 asm(REG(a3)) = *in_out_mr1;
    register seL4_Register msg2 asm(REG(a4)) = *in_out_mr2;
    register seL4_Register msg3 asm(REG(a5)) = *in_out_mr3;
    MCS_PARAM_DECL("a6");

    /* Perform the system call. */
    register seL4_Word scno asm("a7") = sys;
    asm volatile(
        "ecall"
        : "+" ASM_ARG_CONSTR(msg0), "+" ASM_ARG_CONSTR(msg1), "+" ASM_ARG_CONSTR(msg2), "+" ASM_ARG_CONSTR(msg3),
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
static inline void riscv_sys_nbsend_recv(seL4_Word sys, seL4_Word dest, seL4_Word src, seL4_Word *out_badge,
                                         seL4_Word info_arg,
                                         seL4_Word *out_info, seL4_Register *in_out_mr0, seL4_Register *in_out_mr1, seL4_Register *in_out_mr2,
                                         seL4_Register *in_out_mr3, seL4_Word reply)
{
    register seL4_Word src_and_badge asm("a0") = src;
    register seL4_Word info asm("a1") = info_arg;

    /* Load the beginning of the message info registers */
    register seL4_Register msg0 asm(REG(a2)) = *in_out_mr0;
    register seL4_Register msg1 asm(REG(a3)) = *in_out_mr1;
    register seL4_Register msg2 asm(REG(a4)) = *in_out_mr2;
    register seL4_Register msg3 asm(REG(a5)) = *in_out_mr3;

    register seL4_Word reply_reg asm("a6") = reply;
    register seL4_Word dest_reg asm("t0") = dest;

    /* Perform the system call. */
    register seL4_Word scno asm("a7") = sys;
    asm volatile(
        "ecall"
        : "+" ASM_ARG_CONSTR(msg0), "+" ASM_ARG_CONSTR(msg1), "+" ASM_ARG_CONSTR(msg2), "+" ASM_ARG_CONSTR(msg3),
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

LIBSEL4_INLINE_FUNC void seL4_Send(seL4_CPtr dest, seL4_MessageInfo_t msgInfo)
{
    riscv_sys_send(seL4_SysSend, dest, msgInfo.words[0], seL4_GetMR(0), seL4_GetMR(1),
                   seL4_GetMR(2), seL4_GetMR(3));
}

LIBSEL4_INLINE_FUNC void seL4_SendWithMRs(seL4_CPtr dest, seL4_MessageInfo_t msgInfo,
                                          seL4_Register *mr0, seL4_Register *mr1, seL4_Register *mr2, seL4_Register *mr3)
{
    riscv_sys_send(seL4_SysSend, dest, msgInfo.words[0],
                   mr0 != seL4_Null && seL4_MessageInfo_get_length(msgInfo) > 0 ? *mr0 : 0,
                   mr1 != seL4_Null && seL4_MessageInfo_get_length(msgInfo) > 0 ? *mr1 : 0,
                   mr2 != seL4_Null && seL4_MessageInfo_get_length(msgInfo) > 0 ? *mr2 : 0,
                   mr3 != seL4_Null && seL4_MessageInfo_get_length(msgInfo) > 0 ? *mr3 : 0
                  );

}

LIBSEL4_INLINE_FUNC void seL4_NBSend(seL4_CPtr dest, seL4_MessageInfo_t msgInfo)
{
    riscv_sys_send(seL4_SysNBSend, dest, msgInfo.words[0], seL4_GetMR(0), seL4_GetMR(1),
                   seL4_GetMR(2), seL4_GetMR(3));

}

LIBSEL4_INLINE_FUNC void seL4_NBSendWithMRs(seL4_CPtr dest, seL4_MessageInfo_t msgInfo,
                                            seL4_Register *mr0, seL4_Register *mr1, seL4_Register *mr2, seL4_Register *mr3)
{
    riscv_sys_send(seL4_SysNBSend, dest, msgInfo.words[0],
                   mr0 != seL4_Null && seL4_MessageInfo_get_length(msgInfo) > 0 ? *mr0 : 0,
                   mr1 != seL4_Null && seL4_MessageInfo_get_length(msgInfo) > 0 ? *mr1 : 0,
                   mr2 != seL4_Null && seL4_MessageInfo_get_length(msgInfo) > 0 ? *mr2 : 0,
                   mr3 != seL4_Null && seL4_MessageInfo_get_length(msgInfo) > 0 ? *mr3 : 0
                  );

}

#ifndef CONFIG_KERNEL_MCS
LIBSEL4_INLINE_FUNC void seL4_Reply(seL4_MessageInfo_t msgInfo)
{
    riscv_sys_reply(seL4_SysReply, msgInfo.words[0], seL4_GetMR(0), seL4_GetMR(1), seL4_GetMR(2),
                    seL4_GetMR(3));
}

LIBSEL4_INLINE_FUNC void seL4_ReplyWithMRs(seL4_MessageInfo_t msgInfo,
                                           seL4_Register *mr0, seL4_Register *mr1, seL4_Register *mr2, seL4_Register *mr3)
{
    riscv_sys_reply(seL4_SysReply, msgInfo.words[0],
                    mr0 != seL4_Null && seL4_MessageInfo_get_length(msgInfo) > 0 ? *mr0 : 0,
                    mr1 != seL4_Null && seL4_MessageInfo_get_length(msgInfo) > 0 ? *mr1 : 0,
                    mr2 != seL4_Null && seL4_MessageInfo_get_length(msgInfo) > 0 ? *mr2 : 0,
                    mr3 != seL4_Null && seL4_MessageInfo_get_length(msgInfo) > 0 ? *mr3 : 0
                   );

}
#endif

LIBSEL4_INLINE_FUNC void seL4_Signal(seL4_CPtr dest)
{
    riscv_sys_send_null(seL4_SysSend, dest, seL4_MessageInfo_new(0, 0, 0, 0).words[0]);
}

#ifdef CONFIG_KERNEL_MCS
LIBSEL4_INLINE_FUNC seL4_MessageInfo_t seL4_Recv(seL4_CPtr src, seL4_Word *sender, seL4_CPtr reply)
#else
LIBSEL4_INLINE_FUNC seL4_MessageInfo_t seL4_Recv(seL4_CPtr src, seL4_Word *sender)
#endif
{
    seL4_MessageInfo_t info;
    seL4_Word badge;
    seL4_Register msg0;
    seL4_Register msg1;
    seL4_Register msg2;
    seL4_Register msg3;

    riscv_sys_recv(seL4_SysRecv, src, &badge, &info.words[0], &msg0, &msg1, &msg2, &msg3, LIBSEL4_MCS_REPLY);

    seL4_SetMR(0, msg0);
    seL4_SetMR(1, msg1);
    seL4_SetMR(2, msg2);
    seL4_SetMR(3, msg3);

    /* Return back sender and message information. */
    if (sender) {
        *sender = badge;
    }
    return info;

}

#ifdef CONFIG_KERNEL_MCS
LIBSEL4_INLINE_FUNC seL4_MessageInfo_t seL4_RecvWithMRs(seL4_CPtr src, seL4_Word *sender, seL4_CPtr reply,
                                                        seL4_Register *mr0, seL4_Register *mr1, seL4_Register *mr2, seL4_Register *mr3)
#else
LIBSEL4_INLINE_FUNC seL4_MessageInfo_t seL4_RecvWithMRs(seL4_CPtr src, seL4_Word *sender,
                                                        seL4_Register *mr0, seL4_Register *mr1, seL4_Register *mr2, seL4_Register *mr3)
#endif
{
    seL4_MessageInfo_t info;
    seL4_Word badge;
    seL4_Register msg0 = 0;
    seL4_Register msg1 = 0;
    seL4_Register msg2 = 0;
    seL4_Register msg3 = 0;

    riscv_sys_recv(seL4_SysRecv, src, &badge, &info.words[0], &msg0, &msg1, &msg2, &msg3, LIBSEL4_MCS_REPLY);

    /* Write the message back out to memory. */
    if (mr0 != seL4_Null) {
        *mr0 = msg0;
    }
    if (mr1 != seL4_Null) {
        *mr1 = msg1;
    }
    if (mr2 != seL4_Null) {
        *mr2 = msg2;
    }
    if (mr3 != seL4_Null) {
        *mr3 = msg3;
    }

    /* Return back sender and message information. */
    if (sender) {
        *sender = badge;
    }
    return info;
}

#ifdef CONFIG_KERNEL_MCS
LIBSEL4_INLINE_FUNC seL4_MessageInfo_t seL4_NBRecv(seL4_CPtr src, seL4_Word *sender, seL4_CPtr reply)
#else
LIBSEL4_INLINE_FUNC seL4_MessageInfo_t seL4_NBRecv(seL4_CPtr src, seL4_Word *sender)
#endif
{
    seL4_MessageInfo_t info;
    seL4_Word badge;
    seL4_Register msg0;
    seL4_Register msg1;
    seL4_Register msg2;
    seL4_Register msg3;

    riscv_sys_recv(seL4_SysNBRecv, src, &badge, &info.words[0], &msg0, &msg1, &msg2, &msg3, LIBSEL4_MCS_REPLY);

    seL4_SetMR(0, msg0);
    seL4_SetMR(1, msg1);
    seL4_SetMR(2, msg2);
    seL4_SetMR(3, msg3);

    /* Return back sender and message information. */
    if (sender) {
        *sender = badge;
    }
    return info;

}

LIBSEL4_INLINE_FUNC seL4_MessageInfo_t seL4_Call(seL4_CPtr dest, seL4_MessageInfo_t msgInfo)
{
    seL4_MessageInfo_t info;
    seL4_Register msg0 = seL4_GetMR(0);
    seL4_Register msg1 = seL4_GetMR(1);
    seL4_Register msg2 = seL4_GetMR(2);
    seL4_Register msg3 = seL4_GetMR(3);

    riscv_sys_send_recv(seL4_SysCall, dest, &dest, msgInfo.words[0], &info.words[0], &msg0, &msg1,
                        &msg2, &msg3, 0);

    /* Write out the data back to memory. */
    seL4_SetMR(0, msg0);
    seL4_SetMR(1, msg1);
    seL4_SetMR(2, msg2);
    seL4_SetMR(3, msg3);

    return info;
}

#ifndef CONFIG_KERNEL_MCS
LIBSEL4_INLINE_FUNC void seL4_Wait(seL4_CPtr src, seL4_Word *sender)
{
    seL4_Recv(src, sender);
}
#endif

LIBSEL4_INLINE_FUNC seL4_MessageInfo_t seL4_Poll(seL4_CPtr src, seL4_Word *sender)
{
#ifdef CONFIG_KERNEL_MCS
    return seL4_NBWait(src, sender);
#else
    return seL4_NBRecv(src, sender);
#endif
}

LIBSEL4_INLINE_FUNC seL4_MessageInfo_t seL4_CallWithMRs(seL4_CPtr dest, seL4_MessageInfo_t msgInfo,
                                                        seL4_Register *mr0, seL4_Register *mr1, seL4_Register *mr2, seL4_Register *mr3)
{
    seL4_MessageInfo_t info;
    seL4_Register msg0 = 0;
    seL4_Register msg1 = 0;
    seL4_Register msg2 = 0;
    seL4_Register msg3 = 0;

    if (mr0 != seL4_Null && seL4_MessageInfo_get_length(msgInfo) > 0) {
        msg0 = *mr0;
    }
    if (mr1 != seL4_Null && seL4_MessageInfo_get_length(msgInfo) > 1) {
        msg1 = *mr1;
    }
    if (mr2 != seL4_Null && seL4_MessageInfo_get_length(msgInfo) > 2) {
        msg2 = *mr2;
    }
    if (mr3 != seL4_Null && seL4_MessageInfo_get_length(msgInfo) > 3) {
        msg3 = *mr3;
    }

    riscv_sys_send_recv(seL4_SysCall, dest, &dest, msgInfo.words[0], &info.words[0], &msg0, &msg1,
                        &msg2, &msg3, 0);

    if (mr0 != seL4_Null) {
        *mr0 = msg0;
    }
    if (mr1 != seL4_Null) {
        *mr1 = msg1;
    }
    if (mr2 != seL4_Null) {
        *mr2 = msg2;
    }
    if (mr3 != seL4_Null) {
        *mr3 = msg3;
    }

    return info;
}

#ifdef CONFIG_KERNEL_MCS
LIBSEL4_INLINE_FUNC seL4_MessageInfo_t seL4_ReplyRecv(seL4_CPtr src, seL4_MessageInfo_t msgInfo, seL4_Word *sender,
                                                      seL4_CPtr reply)
#else
LIBSEL4_INLINE_FUNC seL4_MessageInfo_t seL4_ReplyRecv(seL4_CPtr src, seL4_MessageInfo_t msgInfo, seL4_Word *sender)
#endif
{
    seL4_MessageInfo_t info;
    seL4_Word badge;
    seL4_Register msg0;
    seL4_Register msg1;
    seL4_Register msg2;
    seL4_Register msg3;

    /* Load beginning of the message into registers. */
    msg0 = seL4_GetMR(0);
    msg1 = seL4_GetMR(1);
    msg2 = seL4_GetMR(2);
    msg3 = seL4_GetMR(3);

    riscv_sys_send_recv(seL4_SysReplyRecv, src, &badge, msgInfo.words[0], &info.words[0], &msg0, &msg1, &msg2, &msg3,
                        LIBSEL4_MCS_REPLY);

    /* Write the message back out to memory. */
    seL4_SetMR(0, msg0);
    seL4_SetMR(1, msg1);
    seL4_SetMR(2, msg2);
    seL4_SetMR(3, msg3);

    /* Return back sender and message information. */
    if (sender) {
        *sender = badge;
    }

    return info;

}

#ifdef CONFIG_KERNEL_MCS
LIBSEL4_INLINE_FUNC seL4_MessageInfo_t seL4_ReplyRecvWithMRs(seL4_CPtr src, seL4_MessageInfo_t msgInfo,
                                                             seL4_Word *sender,
                                                             seL4_Register *mr0, seL4_Register *mr1, seL4_Register *mr2, seL4_Register *mr3, seL4_CPtr reply)
#else
LIBSEL4_INLINE_FUNC seL4_MessageInfo_t seL4_ReplyRecvWithMRs(seL4_CPtr src, seL4_MessageInfo_t msgInfo,
                                                             seL4_Word *sender,
                                                             seL4_Register *mr0, seL4_Register *mr1, seL4_Register *mr2, seL4_Register *mr3)
#endif
{
    seL4_MessageInfo_t info;
    seL4_Word badge;
    seL4_Register msg0 = 0;
    seL4_Register msg1 = 0;
    seL4_Register msg2 = 0;
    seL4_Register msg3 = 0;

    if (mr0 != seL4_Null && seL4_MessageInfo_get_length(msgInfo) > 0) {
        msg0 = *mr0;
    }
    if (mr1 != seL4_Null && seL4_MessageInfo_get_length(msgInfo) > 1) {
        msg1 = *mr1;
    }
    if (mr2 != seL4_Null && seL4_MessageInfo_get_length(msgInfo) > 2) {
        msg2 = *mr2;
    }
    if (mr3 != seL4_Null && seL4_MessageInfo_get_length(msgInfo) > 3) {
        msg3 = *mr3;
    }

    riscv_sys_send_recv(seL4_SysReplyRecv, src, &badge, msgInfo.words[0], &info.words[0], &msg0, &msg1, &msg2, &msg3,
                        LIBSEL4_MCS_REPLY);

    /* Write out the data back to memory. */
    if (mr0 != seL4_Null) {
        *mr0 = msg0;
    }
    if (mr1 != seL4_Null) {
        *mr1 = msg1;
    }
    if (mr2 != seL4_Null) {
        *mr2 = msg2;
    }
    if (mr3 != seL4_Null) {
        *mr3 = msg3;
    }

    /* Return back sender and message information. */
    if (sender) {
        *sender = badge;
    }

    return info;
}

#ifdef CONFIG_KERNEL_MCS
LIBSEL4_INLINE_FUNC seL4_MessageInfo_t seL4_NBSendRecv(seL4_CPtr dest, seL4_MessageInfo_t msgInfo, seL4_CPtr src,
                                                       seL4_Word *sender, seL4_CPtr reply)
{
    seL4_MessageInfo_t info;
    seL4_Word badge;
    seL4_Register msg0;
    seL4_Register msg1;
    seL4_Register msg2;
    seL4_Register msg3;

    /* Load beginning of the message into registers. */
    msg0 = seL4_GetMR(0);
    msg1 = seL4_GetMR(1);
    msg2 = seL4_GetMR(2);
    msg3 = seL4_GetMR(3);

    riscv_sys_nbsend_recv(seL4_SysNBSendRecv, dest, src, &badge, msgInfo.words[0], &info.words[0], &msg0, &msg1, &msg2,
                          &msg3,
                          reply);

    /* Write the message back out to memory. */
    seL4_SetMR(0, msg0);
    seL4_SetMR(1, msg1);
    seL4_SetMR(2, msg2);
    seL4_SetMR(3, msg3);

    /* Return back sender and message information. */
    if (sender) {
        *sender = badge;
    }

    return info;
}

LIBSEL4_INLINE_FUNC seL4_MessageInfo_t seL4_NBSendRecvWithMRs(seL4_CPtr dest, seL4_MessageInfo_t msgInfo, seL4_CPtr src,
                                                              seL4_Word *sender,
                                                              seL4_Register *mr0, seL4_Register *mr1, seL4_Register *mr2, seL4_Register *mr3, seL4_CPtr reply)
{
    seL4_MessageInfo_t info;
    seL4_Word badge;
    seL4_Register msg0 = 0;
    seL4_Register msg1 = 0;
    seL4_Register msg2 = 0;
    seL4_Register msg3 = 0;

    if (mr0 != seL4_Null && seL4_MessageInfo_get_length(msgInfo) > 0) {
        msg0 = *mr0;
    }
    if (mr1 != seL4_Null && seL4_MessageInfo_get_length(msgInfo) > 1) {
        msg1 = *mr1;
    }
    if (mr2 != seL4_Null && seL4_MessageInfo_get_length(msgInfo) > 2) {
        msg2 = *mr2;
    }
    if (mr3 != seL4_Null && seL4_MessageInfo_get_length(msgInfo) > 3) {
        msg3 = *mr3;
    }

    riscv_sys_nbsend_recv(seL4_SysNBSendRecv, dest, src, &badge, msgInfo.words[0], &info.words[0], &msg0, &msg1, &msg2,
                          &msg3,
                          reply);

    /* Write out the data back to memory. */
    if (mr0 != seL4_Null) {
        *mr0 = msg0;
    }
    if (mr1 != seL4_Null) {
        *mr1 = msg1;
    }
    if (mr2 != seL4_Null) {
        *mr2 = msg2;
    }
    if (mr3 != seL4_Null) {
        *mr3 = msg3;
    }

    /* Return back sender and message information. */
    if (sender) {
        *sender = badge;
    }

    return info;
}

LIBSEL4_INLINE_FUNC seL4_MessageInfo_t seL4_NBSendWait(seL4_CPtr dest, seL4_MessageInfo_t msgInfo, seL4_CPtr src,
                                                       seL4_Word *sender)
{
    seL4_MessageInfo_t info;
    seL4_Word badge;
    seL4_Register msg0;
    seL4_Register msg1;
    seL4_Register msg2;
    seL4_Register msg3;

    /* Load beginning of the message into registers. */
    msg0 = seL4_GetMR(0);
    msg1 = seL4_GetMR(1);
    msg2 = seL4_GetMR(2);
    msg3 = seL4_GetMR(3);

    riscv_sys_nbsend_recv(seL4_SysNBSendWait, 0, src, &badge, msgInfo.words[0], &info.words[0], &msg0, &msg1, &msg2, &msg3,
                          dest);

    /* Write the message back out to memory. */
    seL4_SetMR(0, msg0);
    seL4_SetMR(1, msg1);
    seL4_SetMR(2, msg2);
    seL4_SetMR(3, msg3);

    /* Return back sender and message information. */
    if (sender) {
        *sender = badge;
    }

    return info;
}

LIBSEL4_INLINE_FUNC seL4_MessageInfo_t seL4_NBSendWaitWithMRs(seL4_CPtr dest, seL4_MessageInfo_t msgInfo, seL4_CPtr src,
                                                              seL4_Word *sender,
                                                              seL4_Register *mr0, seL4_Register *mr1, seL4_Register *mr2, seL4_Register *mr3)
{
    seL4_MessageInfo_t info;
    seL4_Word badge;
    seL4_Register msg0 = 0;
    seL4_Register msg1 = 0;
    seL4_Register msg2 = 0;
    seL4_Register msg3 = 0;

    if (mr0 != seL4_Null && seL4_MessageInfo_get_length(msgInfo) > 0) {
        msg0 = *mr0;
    }
    if (mr1 != seL4_Null && seL4_MessageInfo_get_length(msgInfo) > 1) {
        msg1 = *mr1;
    }
    if (mr2 != seL4_Null && seL4_MessageInfo_get_length(msgInfo) > 2) {
        msg2 = *mr2;
    }
    if (mr3 != seL4_Null && seL4_MessageInfo_get_length(msgInfo) > 3) {
        msg3 = *mr3;
    }

    riscv_sys_nbsend_recv(seL4_SysNBSendRecv, 0, src, &badge, msgInfo.words[0], &info.words[0], &msg0, &msg1, &msg2, &msg3,
                          dest);

    /* Write out the data back to memory. */
    if (mr0 != seL4_Null) {
        *mr0 = msg0;
    }
    if (mr1 != seL4_Null) {
        *mr1 = msg1;
    }
    if (mr2 != seL4_Null) {
        *mr2 = msg2;
    }
    if (mr3 != seL4_Null) {
        *mr3 = msg3;
    }

    /* Return back sender and message information. */
    if (sender) {
        *sender = badge;
    }

    return info;
}
#endif

LIBSEL4_INLINE_FUNC void seL4_Yield(void)
{
    register seL4_Word scno asm("a7") = seL4_SysYield;
    asm volatile("ecall" :: "r"(scno));
}

#ifdef CONFIG_KERNEL_MCS
LIBSEL4_INLINE_FUNC seL4_MessageInfo_t seL4_Wait(seL4_CPtr src, seL4_Word *sender)
{
    seL4_MessageInfo_t info;
    seL4_Word badge;
    seL4_Register msg0;
    seL4_Register msg1;
    seL4_Register msg2;
    seL4_Register msg3;

    riscv_sys_recv(seL4_SysWait, src, &badge, &info.words[0], &msg0, &msg1, &msg2, &msg3, 0);

    seL4_SetMR(0, msg0);
    seL4_SetMR(1, msg1);
    seL4_SetMR(2, msg2);
    seL4_SetMR(3, msg3);

    /* Return back sender and message information. */
    if (sender) {
        *sender = badge;
    }
    return info;
}

LIBSEL4_INLINE_FUNC seL4_MessageInfo_t seL4_WaitWithMRs(seL4_CPtr src, seL4_Word *sender,
                                                        seL4_Register *mr0, seL4_Register *mr1, seL4_Register *mr2, seL4_Register *mr3)
{
    seL4_MessageInfo_t info;
    seL4_Word badge;
    seL4_Register msg0 = 0;
    seL4_Register msg1 = 0;
    seL4_Register msg2 = 0;
    seL4_Register msg3 = 0;

    riscv_sys_recv(seL4_SysWait, src, &badge, &info.words[0], &msg0, &msg1, &msg2, &msg3, 0);

    /* Write the message back out to memory. */
    if (mr0 != seL4_Null) {
        *mr0 = msg0;
    }
    if (mr1 != seL4_Null) {
        *mr1 = msg1;
    }
    if (mr2 != seL4_Null) {
        *mr2 = msg2;
    }
    if (mr3 != seL4_Null) {
        *mr3 = msg3;
    }

    /* Return back sender and message information. */
    if (sender) {
        *sender = badge;
    }
    return info;
}

LIBSEL4_INLINE_FUNC seL4_MessageInfo_t seL4_NBWait(seL4_CPtr src, seL4_Word *sender)
{
    seL4_MessageInfo_t info;
    seL4_Word badge;
    seL4_Register msg0;
    seL4_Register msg1;
    seL4_Register msg2;
    seL4_Register msg3;

    riscv_sys_recv(seL4_SysNBWait, src, &badge, &info.words[0], &msg0, &msg1, &msg2, &msg3, 0);

    seL4_SetMR(0, msg0);
    seL4_SetMR(1, msg1);
    seL4_SetMR(2, msg2);
    seL4_SetMR(3, msg3);

    /* Return back sender and message information. */
    if (sender) {
        *sender = badge;
    }
    return info;
}
#endif

#ifdef CONFIG_PRINTING
LIBSEL4_INLINE_FUNC void seL4_DebugPutChar(char c)
{
    seL4_Word unused0 = 0;
    seL4_Word unused1 = 0;
    seL4_Register unused2 = 0;
    seL4_Register unused3 = 0;
    seL4_Register unused4 = 0;
    seL4_Register unused5 = 0;

    riscv_sys_send_recv(seL4_SysDebugPutChar, c, &unused0, 0, &unused1, &unused2, &unused3,
                        &unused4, &unused5, 0);
}

LIBSEL4_INLINE_FUNC void seL4_DebugPutString(char *str)
{
    for (char *s = str; *s; s++) {
        seL4_DebugPutChar(*s);
    }
    return;
}

LIBSEL4_INLINE_FUNC void seL4_DebugDumpScheduler(void)
{
    seL4_Word unused0 = 0;
    seL4_Word unused1 = 0;
    seL4_Register unused2 = 0;
    seL4_Register unused3 = 0;
    seL4_Register unused4 = 0;
    seL4_Register unused5 = 0;

    riscv_sys_send_recv(seL4_SysDebugDumpScheduler, 0, &unused0, 0, &unused1, &unused2, &unused3,
                        &unused4, &unused5, 0);
}
#endif

#ifdef CONFIG_DEBUG_BUILD
LIBSEL4_INLINE_FUNC void seL4_DebugHalt(void)
{
    register seL4_Word scno asm("a7") = seL4_SysDebugHalt;
    asm volatile("ecall" :: "r"(scno) : "memory");
}

LIBSEL4_INLINE_FUNC void seL4_DebugSnapshot(void)
{
    register seL4_Word scno asm("a7") = seL4_SysDebugSnapshot;
    asm volatile("ecall" ::"r"(scno) : "memory");
}

LIBSEL4_INLINE_FUNC seL4_Uint32 seL4_DebugCapIdentify(seL4_CPtr cap)
{
    seL4_Word unused0 = 0;
    seL4_Register unused1 = 0;
    seL4_Register unused2 = 0;
    seL4_Register unused3 = 0;
    seL4_Register unused4 = 0;

    riscv_sys_send_recv(seL4_SysDebugCapIdentify, cap, &cap, 0, &unused0, &unused1, &unused2,
                        &unused3, &unused4, 0);
    return (seL4_Uint32)cap;
}

char *strcpy(char *, const char *);
LIBSEL4_INLINE_FUNC void seL4_DebugNameThread(seL4_CPtr tcb, const char *name)
{
    strcpy((char *)seL4_GetIPCBuffer()->msg, name);

    seL4_Word unused0 = 0;
    seL4_Word unused1 = 0;
    seL4_Register unused2 = 0;
    seL4_Register unused3 = 0;
    seL4_Register unused4 = 0;
    seL4_Register unused5 = 0;

    riscv_sys_send_recv(seL4_SysDebugNameThread, tcb, &unused0, 0, &unused1, &unused2, &unused3,
                        &unused4, &unused5, 0);
}
#endif

#ifdef CONFIG_DANGEROUS_CODE_INJECTION
LIBSEL4_INLINE_FUNC void seL4_DebugRun(void (* userfn)(void *), void *userarg)
{
    register seL4_Register arg1 asm(REG(a0)) = (seL4_Register)userfn;
    register seL4_Register arg2 asm(REG(a1)) = (seL4_Register)userarg;
    register seL4_Word scno asm("a7") = seL4_SysDebugRun;
    asm volatile("ecall" : "+" ASM_ARG_CONSTR(arg1) : ASM_ARG_CONSTR(arg2), "r"(scno));
}
#endif

#ifdef CONFIG_ENABLE_BENCHMARKS
/* set the log index back to 0 */
LIBSEL4_INLINE_FUNC seL4_Error seL4_BenchmarkResetLog(void)
{
    seL4_Word unused0 = 0;
    seL4_Register unused1 = 0;
    seL4_Register unused2 = 0;
    seL4_Register unused3 = 0;
    seL4_Register unused4 = 0;

    seL4_Register ret;
    riscv_sys_send_recv(seL4_SysBenchmarkResetLog, 0, &ret, 0, &unused0, &unused1, &unused2, &unused3, &unused4, 0);

    return (seL4_Error) ret;
}

LIBSEL4_INLINE_FUNC seL4_Register seL4_BenchmarkFinalizeLog(void)
{
    seL4_Word unused0 = 0;
    seL4_Register unused1 = 0;
    seL4_Register unused2 = 0;
    seL4_Register unused3 = 0;
    seL4_Register unused4 = 0;

    seL4_Register index_ret;
    riscv_sys_send_recv(seL4_SysBenchmarkFinalizeLog, 0, &index_ret, 0, &unused0, &unused1, &unused2, &unused3, &unused4,
                        0);

    return (seL4_Register) index_ret;
}

LIBSEL4_INLINE_FUNC seL4_Error seL4_BenchmarkSetLogBuffer(seL4_Word frame_cptr)
{
    seL4_Word unused0 = 0;
    seL4_Register unused1 = 0;
    seL4_Register unused2 = 0;
    seL4_Register unused3 = 0;
    seL4_Register unused4 = 0;

    riscv_sys_send_recv(seL4_SysBenchmarkSetLogBuffer, frame_cptr, &frame_cptr, 0, &unused0, &unused1, &unused2, &unused3,
                        &unused4, 0);

    return (seL4_Error) frame_cptr;
}

LIBSEL4_INLINE_FUNC void seL4_BenchmarkNullSyscall(void)
{
    riscv_sys_null(seL4_SysBenchmarkNullSyscall);
    asm volatile("" ::: "memory");
}

LIBSEL4_INLINE_FUNC void seL4_BenchmarkFlushCaches(void)
{
    riscv_sys_send_null(seL4_SysBenchmarkFlushCaches, 0, 0);
    asm volatile("" ::: "memory");
}

LIBSEL4_INLINE_FUNC void seL4_BenchmarkFlushL1Caches(seL4_Word cache_type)
{
    riscv_sys_send_null(seL4_SysBenchmarkFlushCaches, 1, cache_type);
    asm volatile("" ::: "memory");
}

#ifdef CONFIG_BENCHMARK_TRACK_UTILISATION
LIBSEL4_INLINE_FUNC void seL4_BenchmarkGetThreadUtilisation(seL4_Word tcb_cptr)
{
    seL4_Word unused0 = 0;
    seL4_Register unused1 = 0;
    seL4_Register unused2 = 0;
    seL4_Register unused3 = 0;
    seL4_Register unused4 = 0;
    seL4_Register unused5 = 0;

    riscv_sys_send_recv(seL4_SysBenchmarkGetThreadUtilisation, tcb_cptr, &unused0, 0, &unused1, &unused2, &unused3,
                        &unused4, &unused5, 0);
}

LIBSEL4_INLINE_FUNC void seL4_BenchmarkResetThreadUtilisation(seL4_Word tcb_cptr)
{
    seL4_Word unused0 = 0;
    seL4_Register unused1 = 0;
    seL4_Register unused2 = 0;
    seL4_Register unused3 = 0;
    seL4_Register unused4 = 0;
    seL4_Register unused5 = 0;

    riscv_sys_send_recv(seL4_SysBenchmarkResetThreadUtilisation, tcb_cptr, &unused0, 0, &unused1, &unused2, &unused3,
                        &unused4, &unused5, 0);
}
#ifdef CONFIG_DEBUG_BUILD
LIBSEL4_INLINE_FUNC void seL4_BenchmarkDumpAllThreadsUtilisation(void)
{
    seL4_Word unused0 = 0;
    seL4_Register unused1 = 0;
    seL4_Register unused2 = 0;
    seL4_Register unused3 = 0;
    seL4_Register unused4 = 0;
    seL4_Register unused5 = 0;

    riscv_sys_send_recv(seL4_SysBenchmarkDumpAllThreadsUtilisation, 0, &unused0, 0, &unused1, &unused2, &unused3, &unused4,
                        &unused5, 0);
}

LIBSEL4_INLINE_FUNC void seL4_BenchmarkResetAllThreadsUtilisation(void)
{
    seL4_Word unused0 = 0;
    seL4_Register unused1 = 0;
    seL4_Register unused2 = 0;
    seL4_Register unused3 = 0;
    seL4_Register unused4 = 0;
    seL4_Register unused5 = 0;

    riscv_sys_send_recv(seL4_SysBenchmarkResetAllThreadsUtilisation, 0, &unused0, 0, &unused1, &unused2, &unused3, &unused4,
                        &unused5, 0);
}
#endif
#endif /* CONFIG_BENCHMARK_TRACK_UTILISATION */
#endif /* CONFIG_ENABLE_BENCHMARKS */

#ifdef CONFIG_SET_TLS_BASE_SELF
LIBSEL4_INLINE_FUNC void seL4_SetTLSBase(seL4_Register tls_base)
{
    riscv_sys_send_null(seL4_SysSetTLSBase, tls_base, 0);
    asm volatile("" ::: "memory");
}
#endif /* CONFIG_SET_TLS_BASE_SELF */
