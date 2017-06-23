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

#ifndef __LIBSEL4_SEL4_ARCH_SYSCALLS_H
#define __LIBSEL4_SEL4_ARCH_SYSCALLS_H

#include <autoconf.h>
#include <sel4/arch/functions.h>
#include <sel4/types.h>

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
 * arm_sys_null: Does not send any registers to the kernel or expect anything to
 *      be returned from the kernel. Used to trigger implicit kernel actions without
 *      any data (e.g. seL4_Yield)
 */

static inline void
arm_sys_send(seL4_Word sys, seL4_Word dest, seL4_Word info_arg, seL4_Word mr0, seL4_Word mr1, seL4_Word mr2, seL4_Word mr3)
{
    register seL4_Word destptr asm("r0") = dest;
    register seL4_Word info asm("r1") = info_arg;

    /* Load beginning of the message into registers. */
    register seL4_Word msg0 asm("r2") = mr0;
    register seL4_Word msg1 asm("r3") = mr1;
    register seL4_Word msg2 asm("r4") = mr2;
    register seL4_Word msg3 asm("r5") = mr3;

    /* Perform the system call. */
    register seL4_Word scno asm("r7") = sys;
    asm volatile (
        "swi $0"
        : "+r" (destptr), "+r" (msg0), "+r" (msg1), "+r" (msg2),
        "+r" (msg3), "+r" (info)
        : "r"(scno)
    );
}

static inline void
arm_sys_reply(seL4_Word sys, seL4_Word info_arg, seL4_Word mr0, seL4_Word mr1, seL4_Word mr2, seL4_Word mr3)
{
    register seL4_Word info asm("r1") = info_arg;

    /* Load beginning of the message into registers. */
    register seL4_Word msg0 asm("r2") = mr0;
    register seL4_Word msg1 asm("r3") = mr1;
    register seL4_Word msg2 asm("r4") = mr2;
    register seL4_Word msg3 asm("r5") = mr3;

    /* Perform the system call. */
    register seL4_Word scno asm("r7") = sys;
    asm volatile (
        "swi $0"
        : "+r" (msg0), "+r" (msg1), "+r" (msg2), "+r" (msg3),
        "+r" (info)
        : "r"(scno)
    );
}

static inline void
arm_sys_send_null(seL4_Word sys, seL4_Word src, seL4_Word info_arg)
{
    register seL4_Word destptr asm("r0") = src;
    register seL4_Word info asm("r1") = info_arg;

    /* Perform the system call. */
    register seL4_Word scno asm("r7") = sys;
    asm volatile (
        "swi $0"
        : "+r" (destptr), "+r" (info)
        : "r"(scno)
    );
}

static inline void
arm_sys_recv(seL4_Word sys, seL4_Word src, seL4_Word *out_badge, seL4_Word *out_info, seL4_Word *out_mr0, seL4_Word *out_mr1, seL4_Word *out_mr2, seL4_Word *out_mr3)
{
    register seL4_Word src_and_badge asm("r0") = src;
    register seL4_Word info asm("r1");

    /* Incoming message registers. */
    register seL4_Word msg0 asm("r2");
    register seL4_Word msg1 asm("r3");
    register seL4_Word msg2 asm("r4");
    register seL4_Word msg3 asm("r5");

    /* Perform the system call. */
    register seL4_Word scno asm("r7") = sys;
    asm volatile (
        "swi $0"
        : "=r" (msg0), "=r" (msg1), "=r" (msg2), "=r" (msg3),
        "=r" (info), "+r" (src_and_badge)
        : "r"(scno)
        : "memory"
    );
    *out_badge = src_and_badge;
    *out_info = info;
    *out_mr0 = msg0;
    *out_mr1 = msg1;
    *out_mr2 = msg2;
    *out_mr3 = msg3;
}

static inline void
arm_sys_send_recv(seL4_Word sys, seL4_Word dest, seL4_Word *out_badge, seL4_Word info_arg, seL4_Word *out_info, seL4_Word *in_out_mr0, seL4_Word *in_out_mr1, seL4_Word *in_out_mr2, seL4_Word *in_out_mr3)
{
    register seL4_Word destptr asm("r0") = dest;
    register seL4_Word info asm("r1") = info_arg;

    /* Load beginning of the message into registers. */
    register seL4_Word msg0 asm("r2") = *in_out_mr0;
    register seL4_Word msg1 asm("r3") = *in_out_mr1;
    register seL4_Word msg2 asm("r4") = *in_out_mr2;
    register seL4_Word msg3 asm("r5") = *in_out_mr3;

    /* Perform the system call. */
    register seL4_Word scno asm("r7") = sys;
    asm volatile (
        "swi $0"
        : "+r" (msg0), "+r" (msg1), "+r" (msg2), "+r" (msg3),
        "+r" (info), "+r" (destptr)
        : "r"(scno)
        : "memory"
    );
    *out_info = info;
    *out_badge = destptr;
    *in_out_mr0 = msg0;
    *in_out_mr1 = msg1;
    *in_out_mr2 = msg2;
    *in_out_mr3 = msg3;
}

static inline void
arm_sys_null(seL4_Word sys)
{
    register seL4_Word scno asm("r7") = sys;
    asm volatile (
        "swi $0"
        : /* no outputs */
        : "r"(scno)
    );
}

#endif /* __LIBSEL4_SEL4_ARCH_SYSCALLS_H */
