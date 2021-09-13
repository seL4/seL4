/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#include <types.h>
#include <api/failures.h>
#include <config.h>

#include <arch/object/interrupt.h>

static exception_t Arch_invokeIRQControl(irq_t irq, cte_t *handlerSlot, cte_t *controlSlot, bool_t trigger)
{
#ifdef HAVE_SET_TRIGGER
    setIRQTrigger(irq, trigger);
#endif
    return invokeIRQControl(irq, handlerSlot, controlSlot);
}

exception_t Arch_decodeIRQControlInvocation(word_t invLabel, word_t length,
                                            cte_t *srcSlot, word_t *buffer)
{
    if (invLabel == ARMIRQIssueIRQHandlerTrigger) {
        if (length < 4 || current_extra_caps.excaprefs[0] == NULL) {
            current_syscall_error.type = seL4_TruncatedMessage;
            return EXCEPTION_SYSCALL_ERROR;
        }

        if (!config_set(HAVE_SET_TRIGGER)) {
            userError("This platform does not support setting the IRQ trigger");
            current_syscall_error.type = seL4_IllegalOperation;
            return EXCEPTION_SYSCALL_ERROR;
        }

        word_t irq_w = getSyscallArg(0, buffer);
        irq_t irq = (irq_t) CORE_IRQ_TO_IRQT(0, irq_w);
        bool_t trigger = !!getSyscallArg(1, buffer);
        word_t index = getSyscallArg(2, buffer);
        word_t depth = getSyscallArg(3, buffer);

        cap_t cnodeCap = current_extra_caps.excaprefs[0]->cap;

        exception_t status = Arch_checkIRQ(irq_w);
        if (status != EXCEPTION_NONE) {
            return status;
        }

#if defined ENABLE_SMP_SUPPORT
        if (IRQ_IS_PPI(irq)) {
            userError("Trying to get a handler on a PPI: use GetTriggerCore.");
            current_syscall_error.type = seL4_IllegalOperation;
            return EXCEPTION_SYSCALL_ERROR;
        }
#endif
        if (isIRQActive(irq)) {
            current_syscall_error.type = seL4_RevokeFirst;
            userError("Rejecting request for IRQ %u. Already active.", (int)IRQT_TO_IRQ(irq));
            return EXCEPTION_SYSCALL_ERROR;
        }

        lookupSlot_ret_t lu_ret = lookupTargetSlot(cnodeCap, index, depth);
        if (lu_ret.status != EXCEPTION_NONE) {
            userError("Target slot for new IRQ Handler cap invalid: cap %lu, IRQ %u.",
                      getExtraCPtr(buffer, 0), (int)IRQT_TO_IRQ(irq));
            return lu_ret.status;
        }

        cte_t *destSlot = lu_ret.slot;

        status = ensureEmptySlot(destSlot);
        if (status != EXCEPTION_NONE) {
            userError("Target slot for new IRQ Handler cap not empty: cap %lu, IRQ %u.",
                      getExtraCPtr(buffer, 0), (int)IRQT_TO_IRQ(irq));
            return status;
        }

        setThreadState(NODE_STATE(ksCurThread), ThreadState_Restart);
        return Arch_invokeIRQControl(irq, destSlot, srcSlot, trigger);
#ifdef ENABLE_SMP_SUPPORT
    } else if (invLabel == ARMIRQIssueIRQHandlerTriggerCore) {
        word_t irq_w = getSyscallArg(0, buffer);
        bool_t trigger = !!getSyscallArg(1, buffer);
        word_t index = getSyscallArg(2, buffer);
        word_t depth = getSyscallArg(3, buffer) & 0xfful;
        seL4_Word target = getSyscallArg(4, buffer);
        cap_t cnodeCap = current_extra_caps.excaprefs[0]->cap;
        exception_t status = Arch_checkIRQ(irq_w);
        irq_t irq = CORE_IRQ_TO_IRQT(target, irq_w);

        if (status != EXCEPTION_NONE) {
            return status;
        }

        if (target >= CONFIG_MAX_NUM_NODES) {
            current_syscall_error.type = seL4_InvalidArgument;
            userError("Target core %lu is invalid.", target);
            return EXCEPTION_SYSCALL_ERROR;
        }

        if (isIRQActive(irq)) {
            current_syscall_error.type = seL4_RevokeFirst;
            userError("Rejecting request for IRQ %u. Already active.", (int)IRQT_TO_IRQ(irq));
            return EXCEPTION_SYSCALL_ERROR;
        }

        lookupSlot_ret_t lu_ret = lookupTargetSlot(cnodeCap, index, depth);
        if (lu_ret.status != EXCEPTION_NONE) {
            userError("Target slot for new IRQ Handler cap invalid: cap %lu, IRQ %u.",
                      getExtraCPtr(buffer, 0), (int)IRQT_TO_IRQ(irq));
            return lu_ret.status;
        }

        cte_t *destSlot = lu_ret.slot;

        status = ensureEmptySlot(destSlot);
        if (status != EXCEPTION_NONE) {
            userError("Target slot for new IRQ Handler cap not empty: cap %lu, IRQ %u.",
                      getExtraCPtr(buffer, 0), (int)IRQT_TO_IRQ(irq));
            return status;
        }

        setThreadState(NODE_STATE(ksCurThread), ThreadState_Restart);

        /* If the IRQ is not a private interrupt, then the role of the syscall is to set
         * target core to which the shared interrupt will be physically delivered.
         */
        if (!IRQ_IS_PPI(irq)) {
            setIRQTarget(irq, target);
        }
        return Arch_invokeIRQControl(irq, destSlot, srcSlot, trigger);
#endif /* ENABLE_SMP_SUPPORT */
    } else {
        current_syscall_error.type = seL4_IllegalOperation;
        return EXCEPTION_SYSCALL_ERROR;
    }
}
