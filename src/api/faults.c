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
#include <api/faults.h>
#include <api/syscall.h>
#include <kernel/thread.h>
#include <machine/debug.h>

static inline unsigned int
setMRs_lookup_failure(tcb_t *receiver, word_t* receiveIPCBuffer,
                      lookup_fault_t luf, unsigned int offset)
{
    word_t lufType = lookup_fault_get_lufType(luf);
    word_t i;

    i = setMR(receiver, receiveIPCBuffer, offset, lufType + 1);

    switch (lufType) {
    case lookup_fault_invalid_root:
        return i;

    case lookup_fault_missing_capability:
        return setMR(receiver, receiveIPCBuffer, offset + 1,
                     lookup_fault_missing_capability_get_bitsLeft(luf));

    case lookup_fault_depth_mismatch:
        setMR(receiver, receiveIPCBuffer, offset + 1,
              lookup_fault_depth_mismatch_get_bitsLeft(luf));
        return setMR(receiver, receiveIPCBuffer, offset + 2,
                     lookup_fault_depth_mismatch_get_bitsFound(luf));

    case lookup_fault_guard_mismatch:
        setMR(receiver, receiveIPCBuffer, offset + 1,
              lookup_fault_guard_mismatch_get_bitsLeft(luf));
        setMR(receiver, receiveIPCBuffer, offset + 2,
              lookup_fault_guard_mismatch_get_guardFound(luf));
        return setMR(receiver, receiveIPCBuffer, offset + 3,
                     lookup_fault_guard_mismatch_get_bitsFound(luf));

    default:
        fail("Invalid lookup failure");
    }
}

static inline void
copyMRsFaultReply(tcb_t *sender, tcb_t *receiver, const register_t
                  message[], word_t length)
{
    word_t i;
    for (i = 0; i < MIN(length, n_msgRegisters); i++) {
        register_t r = message[i];
        word_t v = getRegister(sender, msgRegisters[i]);
        setRegister(receiver, r, sanitiseRegister(r, v));
    }

    if (i < length) {
        word_t *sendBuf = lookupIPCBuffer(false, sender);
        if (sendBuf) {
            for (; i < length; i++) {
                register_t r = message[i];
                word_t v = sendBuf[i + 1];
                setRegister(receiver, r, sanitiseRegister(r, v));
            }
        }
    }
}

static inline void
copyMRsFault(tcb_t *sender, tcb_t *receiver, const register_t message[],
             word_t length, word_t *receiveIPCBuffer)
{
    word_t i;
    for (i = 0; i < MIN(length, n_msgRegisters); i++) {
        setRegister(receiver, msgRegisters[i], getRegister(sender, message[i]));
    }

    if (receiveIPCBuffer) {
        for (; i < length; i++) {
            receiveIPCBuffer[i + 1] = getRegister(sender, message[i]);
        }
    }
}

bool_t
handleFaultReply(tcb_t *receiver, tcb_t *sender)
{
    /* These lookups are moved inward from doReplyTransfer */
    seL4_MessageInfo_t tag = messageInfoFromWord(getRegister(sender, msgInfoRegister));
    word_t label = seL4_MessageInfo_get_label(tag);
    word_t length = seL4_MessageInfo_get_length(tag);
    fault_t fault = receiver->tcbFault;

    switch (fault_get_faultType(fault)) {
    case fault_cap_fault:
        return true;

    case fault_unknown_syscall:
        copyMRsFaultReply(sender, receiver, syscallMessage, MIN(length, n_syscallMessage));
        return (label == 0);

    case fault_user_exception:
        copyMRsFaultReply(sender, receiver, exceptionMessage, MIN(length, n_exceptionMessage));
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
            /* A single-step reply doesn't mean much if it isn't composed of the bp
             * number and number of instructions to skip. But even if both aren't
             * set, we can still allow the thread to continue because replying
             * should uniformly resume thread execution, based on the general seL4
             * API model.
             *
             * If it was single-step, but no reply registers were set, just
             * default to skipping 1 and continuing.
             *
             * On x86, bp_num actually doesn't matter for single-stepping
             * because single-stepping doesn't use a hardware register -- it
             * uses EFLAGS.TF.
             */
            n_instrs = 1;
        } else {
            /* If the reply had all expected registers set, proceed as normal */
            n_instrs = getRegister(sender, msgRegisters[0]);
        }

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
#endif

    default:
        return Arch_handleFaultReply(receiver, sender, fault_get_faultType(fault));
    }
}

word_t
setMRs_fault(tcb_t *sender, tcb_t* receiver, word_t *receiveIPCBuffer)
{
    switch (fault_get_faultType(sender->tcbFault)) {
    case fault_cap_fault:
        setMR(receiver, receiveIPCBuffer, 0, getRestartPC(sender));
        setMR(receiver, receiveIPCBuffer, 1u,
              fault_cap_fault_get_address(sender->tcbFault));
        setMR(receiver, receiveIPCBuffer, 2u,
              fault_cap_fault_get_inReceivePhase(sender->tcbFault));
        return setMRs_lookup_failure(receiver, receiveIPCBuffer,
                                     sender->tcbLookupFailure, 3u);

    case fault_unknown_syscall: {
        copyMRsFault(sender, receiver, syscallMessage, n_syscallMessage,
                     receiveIPCBuffer);

        return setMR(receiver, receiveIPCBuffer, n_syscallMessage,
                     fault_unknown_syscall_get_syscallNumber(sender->tcbFault));
    }

    case fault_user_exception: {
        copyMRsFault(sender, receiver, exceptionMessage,
                     n_exceptionMessage, receiveIPCBuffer);
        setMR(receiver, receiveIPCBuffer, n_exceptionMessage,
              fault_user_exception_get_number(sender->tcbFault));
        return setMR(receiver, receiveIPCBuffer, n_exceptionMessage + 1u,
                     fault_user_exception_get_code(sender->tcbFault));
    }

#ifdef CONFIG_HARDWARE_DEBUG_API
    case fault_debug_exception: {
        unsigned int ret;
        word_t reason = fault_debug_exception_get_exceptionReason(sender->tcbFault);

        setMR(receiver, receiveIPCBuffer,
              seL4_DebugException_FaultIP, getRestartPC(sender));
        setMR(receiver, receiveIPCBuffer,
              seL4_DebugException_ExceptionReason, reason);

        if (reason != seL4_SingleStep && reason != seL4_SoftwareBreakRequest) {
            ret = setMR(receiver, receiveIPCBuffer,
                        seL4_DebugException_TriggerAddress,
                        fault_debug_exception_get_breakpointAddress(sender->tcbFault));

            /* Breakpoint messages also set a "breakpoint number" register. */
            ret = setMR(receiver, receiveIPCBuffer,
                        seL4_DebugException_BreakpointNumber,
                        fault_debug_exception_get_breakpointNumber(sender->tcbFault));
        }
        return ret;
    }
#endif /* CONFIG_HARDWARE_DEBUG_API */

    default:
        return Arch_setMRs_fault(sender, receiver, receiveIPCBuffer,
                                 fault_get_faultType(sender->tcbFault));
    }
}
