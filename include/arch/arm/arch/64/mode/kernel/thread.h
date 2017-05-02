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
sanitiseRegister(register_t reg, word_t v, bool_t archInfo)
{
    if (reg == SPSR_EL1) {
        return (v & 0xf0000000) | PSTATE_USER;
    } else {
        return v;
    }
}

static inline bool_t CONST
Arch_getSanitiseRegisterInfo(tcb_t *thread)
{
#ifdef CONFIG_ARM_HYPERVISOR_SUPPORT
    /* When hyp support is implemented on aarch64 it will need to be determined whether
     * a similar 'has VCPU' style knowledge is needed in sanitiseRegister or not */
#error aarch64 support for hypervisor not implemented here
#endif /* CONFIG_ARM_HYPERVISOR_SUPPORT */
    return 0;
}

#endif
