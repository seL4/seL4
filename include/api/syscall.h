/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(GD_GPL)
 */

#ifndef __API_SYSCALL_H
#define __API_SYSCALL_H

#include <machine.h>
#include <api/failures.h>
#include <model/statedata.h>
#include <kernel/vspace.h>
#include <arch/api/syscall.h>

exception_t handleSyscall(syscall_t syscall) VISIBLE;
exception_t handleInterruptEntry(void) VISIBLE;
exception_t handleUnknownSyscall(word_t w) VISIBLE;
exception_t handleUserLevelFault(word_t w_a, word_t w_b) VISIBLE;
exception_t handleVMFaultEvent(vm_fault_type_t vm_faultType) VISIBLE;

static inline word_t PURE
getSyscallArg(unsigned int i, word_t* ipc_buffer)
{
    if (i < n_msgRegisters) {
        return getRegister(ksCurThread, msgRegisters[i]);
    }

    assert(ipc_buffer != NULL);
    return ipc_buffer[i + 1];
}

extern extra_caps_t current_extra_caps;

#endif
