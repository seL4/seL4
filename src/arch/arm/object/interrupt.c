/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(GD_GPL)
 */

#include <types.h>
#include <api/failures.h>

#include <arch/object/interrupt.h>

exception_t
Arch_decodeIRQControlInvocation(word_t label, word_t length,
                                cte_t *srcSlot, extra_caps_t extraCaps,
                                word_t *buffer)
{
    current_syscall_error.type = seL4_IllegalOperation;
    return EXCEPTION_SYSCALL_ERROR;
}


exception_t Arch_decodeIRQControl(word_t label, word_t length, cte_t *srcSlot, extra_caps_t extraCaps, word_t *buffer)
{
    userError("IRQControl: Illegal operation.");
    current_syscall_error.type = seL4_IllegalOperation;
    return EXCEPTION_SYSCALL_ERROR;
}
