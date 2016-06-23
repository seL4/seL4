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

#ifdef CONFIG_ARM_HYPERVISOR_SUPPORT
    case fault_vgic_maintenance:
        return true;
    case fault_vcpu_fault:
        return true;
#endif

    case fault_unknown_syscall: {
        word_t i;
        register_t r;
        word_t v;
        word_t *sendBuf;

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
    }
    return (label == 0);

    case fault_user_exception: {
        word_t i;
        register_t r;
        word_t v;

        /* Assumes n_exceptionMessage <= n_msgRegisters */
        for (i = 0; i < length && i < n_exceptionMessage; i++) {
            r = exceptionMessage[i];
            v = getRegister(sender, msgRegisters[i]);
            setRegister(receiver, r, sanitiseRegister(r, v));
        }
    }
    return (label == 0);

#ifdef CONFIG_HARDWARE_DEBUG_API
    case fault_debug_exception: {
        word_t n_instrs;

        if (fault_debug_exception_get_exceptionReason(fault) != seL4_SingleStep) {
            /* Only single-step replies are required to set message registers.
             */
            return (label == 0);
        }

        if (length < DEBUG_REPLY_N_EXPECTED_REGISTERS) {
            /* If the user didn't set all the expected registers, assume
             * the number of instructions to step is 1.
             */
            n_instrs = 1;
        } else {
            /* If the reply had all expected registers set, proceed as normal */
            n_instrs = getRegister(sender, msgRegisters[0]);
        }

        /* When replying to a single-step fault, default the bp_num to the
         * one that was configured and cached in the TCB context.
         *
         * configureSingleStepping() will know this because we pass "true" to
         * is_reply.
         */
        syscall_error_t res;

        res = Arch_decodeConfigureSingleStepping(&receiver->tcbArch, 0, n_instrs, true);
        if (res.type != seL4_NoError) {
            return false;
        };

        configureSingleStepping(&receiver->tcbArch, 0, n_instrs, true);

        /* Replying will always resume the thread: the only variant behaviour
         * is whether or not the thread will be resumed with stepping still
         * enabled.
         */
        return (label == 0);
    }
#endif /* CONFIG_HARDWARE_DEBUG_API */

    default:
        fail("Invalid fault");
    }
}
