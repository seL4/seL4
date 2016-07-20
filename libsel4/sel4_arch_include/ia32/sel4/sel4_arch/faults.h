/*
 * Copyright 2014, NICTA
 *
 * This software may be distributed and modified according to the terms of
 * the BSD 2-Clause license. Note that NO WARRANTY is provided.
 * See "LICENSE_BSD2.txt" for details.
 *
 * @TAG(NICTA_BSD)
 */

#ifndef __LIBSEL4_SEL4_ARCH_FAULTS_H
#define __LIBSEL4_SEL4_ARCH_FAULTS_H

#include <autoconf.h>
#include <sel4/faults.h>
#include <sel4/sel4_arch/constants.h>

static inline seL4_Fault_t
seL4_getArchFault(seL4_MessageInfo_t tag)
{
    switch (seL4_MessageInfo_get_label(tag)) {
    case seL4_Fault_UnknownSyscall:
        return seL4_Fault_UnknownSyscall_new(seL4_GetMR(seL4_UnknownSyscall_EAX),
                                             seL4_GetMR(seL4_UnknownSyscall_EBX),
                                             seL4_GetMR(seL4_UnknownSyscall_ECX),
                                             seL4_GetMR(seL4_UnknownSyscall_EDX),
                                             seL4_GetMR(seL4_UnknownSyscall_ESI),
                                             seL4_GetMR(seL4_UnknownSyscall_EDI),
                                             seL4_GetMR(seL4_UnknownSyscall_EBP),
                                             seL4_GetMR(seL4_UnknownSyscall_FaultIP),
                                             seL4_GetMR(seL4_UnknownSyscall_SP),
                                             seL4_GetMR(seL4_UnknownSyscall_EFLAGS),
                                             seL4_GetMR(seL4_UnknownSyscall_Syscall));

    case seL4_Fault_UserException:
        return seL4_Fault_UserException_new(seL4_GetMR(seL4_UserException_FaultIP),
                                            seL4_GetMR(seL4_UserException_SP),
                                            seL4_GetMR(seL4_UserException_EFLAGS),
                                            seL4_GetMR(seL4_UserException_Number),
                                            seL4_GetMR(seL4_UserException_Code));
    case seL4_Fault_VMFault:
        return seL4_Fault_VMFault_new(seL4_GetMR(seL4_VMFault_IP),
                                      seL4_GetMR(seL4_VMFault_Addr),
                                      seL4_GetMR(seL4_VMFault_PrefetchFault),
                                      seL4_GetMR(seL4_VMFault_FSR));
    default:
        return seL4_Fault_NullFault_new();
    }
}


#endif
