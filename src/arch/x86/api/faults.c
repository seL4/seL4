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
        unsigned int i;
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
        unsigned int i;
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
    uint32_t vector,
    uint32_t errcode,
    uint32_t eip,
    uint32_t esp,
    uint32_t eflags,
    uint32_t cr0,
    uint32_t cr2,
    uint32_t cr3,
    uint32_t cr4
);

VISIBLE
void handleKernelException(
    uint32_t vector,
    uint32_t errcode,
    uint32_t eip,
    uint32_t esp,
    uint32_t eflags,
    uint32_t cr0,
    uint32_t cr2,
    uint32_t cr3,
    uint32_t cr4
)
{
    unsigned int i;

    printf("\n========== KERNEL EXCEPTION ==========\n");
    printf("Vector:  0x%x\n", vector);
    printf("ErrCode: 0x%x\n", errcode);
    printf("EIP:     0x%x\n", eip);
    printf("ESP:     0x%x\n", esp);
    printf("EFLAGS:  0x%x\n", eflags);
    printf("CR0:     0x%x\n", cr0);
    printf("CR2:     0x%x (page-fault address)\n", cr2);
    printf("CR3:     0x%x (page-directory physical address)\n", cr3);
    printf("CR4:     0x%x\n", cr4);
    printf("\nStack Dump:\n");
    for (i = 0; i < 20; i++) {
        printf("*0x%x == 0x%x\n", esp + i * 4, *(uint32_t*)(esp + i * 4));
    }
    printf("\nHalting...\n");
}

#endif
