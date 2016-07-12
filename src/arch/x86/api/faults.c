/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(GD_GPL)
 */

#include <types.h>
#include <machine/io.h>
#include <api/faults.h>
#include <api/syscall.h>
#include <util.h>

bool_t Arch_handleFaultReply(tcb_t *receiver, tcb_t *sender, word_t faultType)
{
    switch (faultType) {
    case fault_vm_fault:
        return true;

    default:
        fail("Invalid fault");
    }
}

word_t
Arch_setMRs_fault(tcb_t *sender, tcb_t* receiver, word_t *receiveIPCBuffer, word_t faultType)
{
    switch (faultType) {
    case fault_vm_fault: {
        setMR(receiver, receiveIPCBuffer, 0, getRestartPC(sender));
        setMR(receiver, receiveIPCBuffer, 1,
              fault_vm_fault_get_address(sender->tcbFault));
        setMR(receiver, receiveIPCBuffer, 2,
              fault_vm_fault_get_instructionFault(sender->tcbFault));
        return setMR(receiver, receiveIPCBuffer, 3,
                     fault_vm_fault_get_FSR(sender->tcbFault));
    }
    default:
        fail("Invalid fault");
    }
}

#ifdef DEBUG

void handleKernelException(
    word_t vector,
    word_t errcode,
    word_t ip,
    word_t sp,
    word_t flags,
    word_t cr0,
    word_t cr2,
    word_t cr3,
    word_t cr4
);

VISIBLE
void handleKernelException(
    word_t vector,
    word_t errcode,
    word_t ip,
    word_t sp,
    word_t flags,
    word_t cr0,
    word_t cr2,
    word_t cr3,
    word_t cr4
)
{
    word_t i;

    printf("\n========== KERNEL EXCEPTION ==========\n");
    printf("Vector:  0x%lx\n", vector);
    printf("ErrCode: 0x%lx\n", errcode);
    printf("IP:      0x%lx\n", ip);
    printf("SP:      0x%lx\n", sp);
    printf("FLAGS:   0x%lx\n", flags);
    printf("CR0:     0x%lx\n", cr0);
    printf("CR2:     0x%lx (page-fault address)\n", cr2);
    printf("CR3:     0x%lx (page-directory physical address)\n", cr3);
    printf("CR4:     0x%lx\n", cr4);
    printf("\nStack Dump:\n");
    for (i = 0; i < 20; i++) {
        word_t UNUSED stack = sp + i * sizeof(word_t);
        printf("*0x%lx == 0x%lx\n", stack, *(word_t*)stack);
    }
    printf("\nHalting...\n");
}

#endif
