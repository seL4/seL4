/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(GD_GPL)
 */

#include <kernel/boot.h>
#include <model/statedata.h>
#include <arch/object/interrupt.h>
#include <arch/linker.h>

exception_t Arch_decodeIRQControlInvocation(word_t label, word_t length,
                                            cte_t *srcSlot, extra_caps_t extraCaps,
                                            word_t *buffer)
{
    current_syscall_error.type = seL4_IllegalOperation;
    return EXCEPTION_SYSCALL_ERROR;
}
