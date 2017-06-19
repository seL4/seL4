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
#pragma once

#include <autoconf.h>
#include <sel4/faults.h>
#include <sel4/sel4_arch/constants.h>

LIBSEL4_INLINE_FUNC seL4_Fault_t
seL4_getArchFault(seL4_MessageInfo_t tag)
{
    switch (seL4_MessageInfo_get_label(tag)) {
    case seL4_Fault_UnknownSyscall:
        return seL4_Fault_UnknownSyscall_new(seL4_GetMR(seL4_UnknownSyscall_R0),
                                             seL4_GetMR(seL4_UnknownSyscall_R1),
                                             seL4_GetMR(seL4_UnknownSyscall_R2),
                                             seL4_GetMR(seL4_UnknownSyscall_R3),
                                             seL4_GetMR(seL4_UnknownSyscall_R4),
                                             seL4_GetMR(seL4_UnknownSyscall_R5),
                                             seL4_GetMR(seL4_UnknownSyscall_R6),
                                             seL4_GetMR(seL4_UnknownSyscall_R7),
                                             seL4_GetMR(seL4_UnknownSyscall_FaultIP),
                                             seL4_GetMR(seL4_UnknownSyscall_SP),
                                             seL4_GetMR(seL4_UnknownSyscall_LR),
                                             seL4_GetMR(seL4_UnknownSyscall_CPSR),
                                             seL4_GetMR(seL4_UnknownSyscall_Syscall));

    case seL4_Fault_UserException:
        return seL4_Fault_UserException_new(seL4_GetMR(seL4_UserException_FaultIP),
                                            seL4_GetMR(seL4_UserException_SP),
                                            seL4_GetMR(seL4_UserException_CPSR),
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
#endif /* CONFIG_ARM_HYPERVISOR_SUPPORT */
    default:
        return seL4_Fault_NullFault_new();
    }
}

#ifdef CONFIG_ARM_HYPERVISOR_SUPPORT
LIBSEL4_INLINE_FUNC seL4_Bool
seL4_isVGICMaintenance_tag(seL4_MessageInfo_t tag)
{
    return seL4_MessageInfo_get_label(tag) == seL4_Fault_VGICMaintenance;
}

LIBSEL4_INLINE_FUNC seL4_Bool
seL4_isVCPUFault_tag(seL4_MessageInfo_t tag)
{
    return seL4_MessageInfo_get_label(tag) == seL4_Fault_VCPUFault;
}
#endif /* CONFIG_ARM_HYPERVISOR_SUPPORT */
