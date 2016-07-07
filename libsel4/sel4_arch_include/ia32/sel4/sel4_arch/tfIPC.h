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

LIBSEL4_INLINE seL4_MessageInfo_t
seL4_TF_ReplyTag(seL4_Bool resume, seL4_UserContext regs)
{
    seL4_MessageInfo_t tag = seL4_MessageInfo_new(!resume, 0, 0,
                                                  sizeof(seL4_UserContext) / sizeof(seL4_Word));

    /* Marshal input parameters. */
    seL4_SetMR(0, regs.eip);
    seL4_SetMR(1, regs.esp);
    seL4_SetMR(2, regs.eflags);
    seL4_SetMR(3, regs.eax);
    seL4_SetMR(4, regs.ebx);
    seL4_SetMR(5, regs.ecx);
    seL4_SetMR(6, regs.edx);
    seL4_SetMR(7, regs.esi);
    seL4_SetMR(8, regs.edi);
    seL4_SetMR(9, regs.ebp);
    seL4_SetMR(10, regs.tls_base);
    seL4_SetMR(11, regs.fs);
    seL4_SetMR(12, regs.gs);

    return tag;
}

