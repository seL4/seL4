/*
 * Copyright 2014, NICTA
 *
 * This software may be distributed and modified according to the terms of
 * the BSD 2-Clause license. Note that NO WARRANTY is provided.
 * See "LICENSE_BSD2.txt" for details.
 *
 * @TAG(NICTA_BSD)
 */

#pragma once

#include <sel4/types.h>

LIBSEL4_INLINE seL4_MessageInfo_t
seL4_TF_ReplyTag(seL4_Bool resume, seL4_UserContext regs)
{
    seL4_MessageInfo_t tag = seL4_MessageInfo_new(!resume, 0, 0,
                                                  sizeof(seL4_UserContext) / sizeof(seL4_Word));

    /* Marshal input parameters. */
    seL4_SetMR(0, regs.pc);
    seL4_SetMR(1, regs.sp);
    seL4_SetMR(2, regs.cpsr);
    seL4_SetMR(3, regs.r0);
    seL4_SetMR(4, regs.r1);
    seL4_SetMR(5, regs.r8);
    seL4_SetMR(6, regs.r9);
    seL4_SetMR(7, regs.r10);
    seL4_SetMR(8, regs.r11);
    seL4_SetMR(9, regs.r12);
    seL4_SetMR(10, regs.r2);
    seL4_SetMR(11, regs.r3);
    seL4_SetMR(12, regs.r4);
    seL4_SetMR(13, regs.r5);
    seL4_SetMR(14, regs.r6);
    seL4_SetMR(15, regs.r7);
    seL4_SetMR(16, regs.r14);

    return tag;
}


