/*
 * Copyright 2017, General Dynamics C4 Systems
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(GD_GPL)
 */

#ifndef __MODE_KERNEL_THREAD_H
#define __MODE_KERNEL_THREAD_H

static inline word_t CONST
sanitiseRegister(register_t reg, word_t v, tcb_t *thread_arch)
{
    if (reg == SPSR_EL1) {
        return (v & 0xf0000000) | PSTATE_USER;
    } else {
        return v;
    }
}

#endif
