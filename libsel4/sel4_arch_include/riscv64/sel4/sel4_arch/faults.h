/*
 * Copyright 2018, Data61
 * Commonwealth Scientific and Industrial Research Organisation (CSIRO)
 * ABN 41 687 119 230.
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(DATA61_GPL)
 */

/*
 *
 * Copyright 2016, 2017 Hesham Almatary, Data61/CSIRO <hesham.almatary@data61.csiro.au>
 * Copyright 2015, 2016 Hesham Almatary <heshamelmatary@gmail.com>
 */

#ifndef __LIBSEL4_SEL4_ARCH_FAULTS_H
#define __LIBSEL4_SEL4_ARCH_FAULTS_H

#include <autoconf.h>
#include <sel4/faults.h>
#include <sel4/sel4_arch/constants.h>

LIBSEL4_INLINE_FUNC seL4_Fault_t
seL4_getArchFault(seL4_MessageInfo_t tag)
{
    switch (seL4_MessageInfo_get_label(tag)) {
    case seL4_Fault_UnknownSyscall:
        return seL4_Fault_UnknownSyscall_new(
                   seL4_GetMR(seL4_UnknownSyscall_FaultIP),
                   seL4_GetMR(seL4_UnknownSyscall_SP),
                   seL4_GetMR(seL4_UnknownSyscall_RA),
                   seL4_GetMR(seL4_UnknownSyscall_A0),
                   seL4_GetMR(seL4_UnknownSyscall_A1),
                   seL4_GetMR(seL4_UnknownSyscall_A2),
                   seL4_GetMR(seL4_UnknownSyscall_A3),
                   seL4_GetMR(seL4_UnknownSyscall_A4),
                   seL4_GetMR(seL4_UnknownSyscall_A5),
                   seL4_GetMR(seL4_UnknownSyscall_A6),
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
    default:
        return seL4_Fault_NullFault_new();
    }
}

#endif /* __LIBSEL4_SEL4_ARCH_FAULTS_H */
