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
        return seL4_Fault_UnknownSyscall_new(seL4_GetMR(seL4_UnknownSyscall_RAX),
                                             seL4_GetMR(seL4_UnknownSyscall_RBX),
                                             seL4_GetMR(seL4_UnknownSyscall_RCX),
                                             seL4_GetMR(seL4_UnknownSyscall_RDX),
                                             seL4_GetMR(seL4_UnknownSyscall_RSI),
                                             seL4_GetMR(seL4_UnknownSyscall_RDI),
                                             seL4_GetMR(seL4_UnknownSyscall_RBP),
                                             seL4_GetMR(seL4_UnknownSyscall_R8),
                                             seL4_GetMR(seL4_UnknownSyscall_R9),
                                             seL4_GetMR(seL4_UnknownSyscall_R10),
                                             seL4_GetMR(seL4_UnknownSyscall_R11),
                                             seL4_GetMR(seL4_UnknownSyscall_R12),
                                             seL4_GetMR(seL4_UnknownSyscall_R13),
                                             seL4_GetMR(seL4_UnknownSyscall_R14),
                                             seL4_GetMR(seL4_UnknownSyscall_R15),
                                             seL4_GetMR(seL4_UnknownSyscall_FaultIP),
                                             seL4_GetMR(seL4_UnknownSyscall_SP),
                                             seL4_GetMR(seL4_UnknownSyscall_FLAGS),
                                             seL4_GetMR(seL4_UnknownSyscall_Syscall));

    case seL4_Fault_UserException:
        return seL4_Fault_UserException_new(seL4_GetMR(seL4_UserException_FaultIP),
                                            seL4_GetMR(seL4_UserException_SP),
                                            seL4_GetMR(seL4_UserException_FLAGS),
                                            seL4_GetMR(seL4_UserException_Number),
                                            seL4_GetMR(seL4_UserException_Code));
    case seL4_Fault_VMFault:
        return seL4_Fault_VMFault_new(seL4_GetMR(seL4_VMFault_IP),
                                      seL4_GetMR(seL4_VMFault_Addr),
                                      seL4_GetMR(seL4_VMFault_PrefetchFault),
                                      seL4_GetMR(seL4_VMFault_FSR));
#ifdef CONFIG_KERNEL_MCS
    case seL4_Fault_Timeout:
        return seL4_Fault_Timeout_new(seL4_GetMR(seL4_Timeout_Data),
                                      seL4_GetMR(seL4_Timeout_Consumed));
#endif /* CONFIG_KERNEL_MCS */
    default:
        return seL4_Fault_NullFault_new();
    }
}

