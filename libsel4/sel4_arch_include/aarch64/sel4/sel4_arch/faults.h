/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#pragma once

#include <sel4/config.h>
#include <sel4/faults.h>
#include <sel4/sel4_arch/constants.h>

LIBSEL4_INLINE_FUNC seL4_Fault_t seL4_getArchFault(seL4_MessageInfo_t tag)
{
    switch (seL4_MessageInfo_get_label(tag)) {
    case seL4_Fault_UnknownSyscall:
        return seL4_Fault_UnknownSyscall_new(seL4_GetMR(seL4_UnknownSyscall_X0),
                                             seL4_GetMR(seL4_UnknownSyscall_X1),
                                             seL4_GetMR(seL4_UnknownSyscall_X2),
                                             seL4_GetMR(seL4_UnknownSyscall_X3),
                                             seL4_GetMR(seL4_UnknownSyscall_X4),
                                             seL4_GetMR(seL4_UnknownSyscall_X5),
                                             seL4_GetMR(seL4_UnknownSyscall_X6),
                                             seL4_GetMR(seL4_UnknownSyscall_X7),
                                             seL4_GetMR(seL4_UnknownSyscall_FaultIP),
                                             seL4_GetMR(seL4_UnknownSyscall_SP),
                                             seL4_GetMR(seL4_UnknownSyscall_LR),
                                             seL4_GetMR(seL4_UnknownSyscall_SPSR),
                                             seL4_GetMR(seL4_UnknownSyscall_Syscall));

    case seL4_Fault_UserException:
        return seL4_Fault_UserException_new(seL4_GetMR(seL4_UserException_FaultIP),
                                            seL4_GetMR(seL4_UserException_SP),
                                            seL4_GetMR(seL4_UserException_SPSR),
                                            seL4_GetMR(seL4_UserException_Number),
                                            seL4_GetMR(seL4_UserException_Code));
    case seL4_Fault_VMFault:
        return seL4_Fault_VMFault_new(seL4_GetMR(seL4_VMFault_IP),
                                      seL4_GetMR(seL4_VMFault_Addr),
                                      seL4_GetMR(seL4_VMFault_PrefetchFault),
                                      seL4_GetMR(seL4_VMFault_FSR));
#ifdef CONFIG_ARM_HYPERVISOR_SUPPORT
    case seL4_Fault_VGICMaintenance:
        return seL4_Fault_VGICMaintenance_new(seL4_GetMR(seL4_VGICMaintenance_IDX));
    case seL4_Fault_VCPUFault:
        return seL4_Fault_VCPUFault_new(seL4_GetMR(seL4_VCPUFault_HSR));
    case seL4_Fault_VPPIEvent:
        return seL4_Fault_VPPIEvent_new(seL4_GetMR(seL4_VPPIEvent_IRQ));
#endif /* CONFIG_ARM_HYPERVISOR_SUPPORT */
#ifdef CONFIG_KERNEL_MCS
    case seL4_Fault_Timeout:
        return seL4_Fault_Timeout_new(seL4_GetMR(seL4_Timeout_Data),
                                      seL4_GetMR(seL4_Timeout_Consumed));
#endif /* CONFIG_KERNEL_MCS */
    default:
        return seL4_Fault_NullFault_new();
    }
}
