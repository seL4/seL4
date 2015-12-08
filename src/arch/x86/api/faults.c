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
#include <object.h>
#include <machine/io.h>
#include <kernel/vspace.h>
#include <api/faults.h>
#include <api/syscall.h>
#include <util.h>

bool_t handleFaultReply(tcb_t *receiver, tcb_t *sender)
{
    message_info_t tag;
    word_t         label;
    fault_t        fault;
    unsigned int   length;

    /* These lookups are moved inward from doReplyTransfer */
    tag = messageInfoFromWord(getRegister(sender, msgInfoRegister));
    label = message_info_get_msgLabel(tag);
    length = message_info_get_msgLength(tag);
    fault = receiver->tcbFault;

    switch (fault_get_faultType(fault)) {
    case fault_cap_fault:
        return true;

    case fault_vm_fault:
        return true;

    case fault_unknown_syscall: {
        word_t i;
        register_t   r;
        word_t       v;
        word_t*      sendBuf;

        sendBuf = lookupIPCBuffer(false, sender);

        /* Assumes n_syscallMessage > n_msgRegisters */
        for (i = 0; i < length && i < n_msgRegisters; i++) {
            r = syscallMessage[i];
            v = getRegister(sender, msgRegisters[i]);
            setRegister(receiver, r, sanitiseRegister(r, v));
        }

        if (sendBuf) {
            for (; i < length && i < n_syscallMessage; i++) {
                r = syscallMessage[i];
                v = sendBuf[i + 1];
                setRegister(receiver, r, sanitiseRegister(r, v));
            }
        }
        /* HACK: Copy NextEIP to FaultEIP because FaultEIP will be copied */
        /* back to NextEIP later on (and we don't wanna lose NextEIP)     */
        setRegister(receiver, FaultEIP, getRegister(receiver, NextEIP));
    }
    return (label == 0);

    case fault_user_exception: {
        word_t i;
        register_t   r;
        word_t       v;
        word_t*      sendBuf;

        sendBuf = lookupIPCBuffer(false, sender);

        /* Assumes n_exceptionMessage > n_msgRegisters */
        for (i = 0; i < length && i < n_msgRegisters; i++) {
            r = exceptionMessage[i];
            v = getRegister(sender, msgRegisters[i]);
            setRegister(receiver, r, sanitiseRegister(r, v));
        }

        if (sendBuf) {
            for (; i < length && i < n_exceptionMessage; i++) {
                r = exceptionMessage[i];
                v = sendBuf[i + 1];
                setRegister(receiver, r, sanitiseRegister(r, v));
            }
        }
    }
    return (label == 0);

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

extern char kernel_stack_alloc[];

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
        word_t stack = sp + i * sizeof(word_t);
        printf("*0x%lx == 0x%lx\n", stack, *(word_t*)stack);
    }
    printf("\nHalting...\n");
}

#endif
