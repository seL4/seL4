/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(GD_GPL)
 */

#include <config.h>
#include <types.h>
#include <object.h>
#include <kernel/vspace.h>
#include <api/faults.h>
#include <api/syscall.h>

bool_t
handleFaultReply(tcb_t *receiver, tcb_t *sender)
{
    seL4_MessageInfo_t tag;
    word_t label;
    fault_t fault;
    word_t length;

    /* These lookups are moved inward from doReplyTransfer */
    tag = messageInfoFromWord(getRegister(sender, msgInfoRegister));
    label = seL4_MessageInfo_get_label(tag);
    length = seL4_MessageInfo_get_length(tag);
    fault = receiver->tcbFault;

    switch (fault_get_faultType(fault)) {
    case fault_cap_fault:
        return true;

    case fault_vm_fault:
        return true;

    case fault_temporal:
        copyMessageToRegisters(sender, receiver, temporalMessage, MIN(length, n_temporalMessage));
        return (label == 0);
#ifdef CONFIG_ARM_HYPERVISOR_SUPPORT
    case fault_vgic_maintenance:
        return true;
    case fault_vcpu_fault:
        return true;
#endif

    case fault_unknown_syscall:
        copyMessageToRegisters(sender, receiver, syscallMessage, MIN(length, n_syscallMessage));
        return (label == 0);

    case fault_user_exception:
        copyMessageToRegisters(sender, receiver, exceptionMessage, MIN(length, n_exceptionMessage));
        return (label == 0);

    default:
        fail("Invalid fault");
    }
}
