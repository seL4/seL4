/*
 * Copyright 2017, General Dynamics C4 Systems
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#pragma once

static inline register_t CONST sanitiseRegister(regoff_t reg, register_t v, bool_t archInfo)
{
    if (reg == SPSR_EL1) {
        if (archInfo) {
            switch (v & 0x1f) {
            case PMODE_EL0t:
            case PMODE_EL1t:
            case PMODE_EL1h:
                return v;
            default:
                break;
            }
        }
        return (v & 0xf0000000) | PSTATE_USER;
    } else {
        return v;
    }
}

static inline bool_t CONST Arch_getSanitiseRegisterInfo(tcb_t *thread)
{
#ifdef CONFIG_ARM_HYPERVISOR_SUPPORT
    return (thread->tcbArch.tcbVCPU != NULL);
#else
    return 0;
#endif /* CONFIG_ARM_HYPERVISOR_SUPPORT */
}

