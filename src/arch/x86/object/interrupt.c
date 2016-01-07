/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(GD_GPL)
 */

#include <kernel/boot.h>
#include <model/statedata.h>
#include <arch/object/interrupt.h>
#include <arch/api/invocation.h>
#include <arch/linker.h>
#include <plat/machine/pci.h>

void
Arch_irqStateInit(void)
{
    int i = 0;
    for (i = 0; i <= maxIRQ; i++) {
        if (i == irq_timer || i == irq_iommu) {
            x86KSIRQState[i] = x86_irq_state_irq_reserved_new();
        } else {
            x86KSIRQState[i] = x86_irq_state_irq_free_new();
        }
    }
}

/* for x86, the IRQIssueIRQHandler is only allowed to
 * issue a hander for IRQ 0-15, the isa IRQs.
 * Use getIRQHandlerIOAPIC and getIRQHandlerMSI for
 * the IRQs >= 16. Additionally these IRQs only exist
 * if using the legacy PIC interrupt
 */
exception_t
Arch_checkIRQ(word_t irq)
{
    if (config_set(CONFIG_IRQ_PIC) && irq >= irq_isa_min && irq <= irq_isa_max) {
        return EXCEPTION_NONE;
    }
    if (config_set(CONFIG_IRQ_IOAPIC)) {
        userError("IRQControl: Illegal operation");
        current_syscall_error.type = seL4_IllegalOperation;
    } else {
        userError("IRQControl: IRQ %ld should in range %ld - %ld", irq, (long)irq_isa_min, (long)irq_isa_max);
        current_syscall_error.type = seL4_RangeError;
        current_syscall_error.rangeErrorMin = irq_isa_min;
        current_syscall_error.rangeErrorMax = irq_isa_max;
    }
    return EXCEPTION_SYSCALL_ERROR;
}

static void inline
updateIRQState(word_t irq, x86_irq_state_t state)
{
    assert(irq >= 0 && irq <= maxIRQ);
    x86KSIRQState[irq] = state;
}

void
Arch_updateIRQState(word_t irq, x86_irq_state_t state)
{
    updateIRQState(irq, state);
}

void
maskInterrupt(bool_t disable, irq_t irq)
{
    if (irq >= irq_isa_min && irq <= irq_isa_max) {
        if (config_set(CONFIG_IRQ_PIC)) {
            pic_mask_irq(disable, irq);
        } else {
            /* We shouldn't receive interrupts on the PIC range
             * if not using the PIC, but soldier on anyway */
        }
    } else if (irq >= irq_user_min && irq <= irq_user_max) {
        x86_irq_state_t state = x86KSIRQState[irq];
        switch (x86_irq_state_get_irqType(state)) {
        case x86_irq_state_irq_ioapic: {
            uint32_t ioapic = x86_irq_state_irq_ioapic_get_id(state);
            uint32_t pin = x86_irq_state_irq_ioapic_get_pin(state);
            ioapic_mask(disable, ioapic, pin);
            state =  x86_irq_state_irq_ioapic_set_masked(state, disable);
            Arch_updateIRQState(irq, state);
            }
            break;
        case x86_irq_state_irq_msi:
            /* currently MSI interrupts can not be disabled */
            break;
        case x86_irq_state_irq_free:
            /* A spurious interrupt, and the resulting mask here,
             * could be from a user ripping out a vector before
             * the interrupt reached the kernel. Silently ignore */
            break;
        }
    } else {
        /* masking some other kind of interrupt source, this probably
         * shouldn't happen, but soldier on */
    }
}

static exception_t
Arch_invokeIRQControl(irq_t irq, cte_t *handlerSlot, cte_t *controlSlot, x86_irq_state_t irqState)
{
    updateIRQState(irq, irqState);
    return invokeIRQControl(irq, handlerSlot, controlSlot);
}

static exception_t
invokeIssueIRQHandlerIOAPIC(irq_t irq, word_t ioapic, word_t pin, word_t level, word_t polarity, word_t vector,
                            cte_t *handlerSlot, cte_t *controlSlot)
{
    x86_irq_state_t irqState = x86_irq_state_irq_ioapic_new(ioapic, pin, level, polarity, 1);
    ioapic_map_pin_to_vector(ioapic, pin, level, polarity, vector);
    return Arch_invokeIRQControl(irq, handlerSlot, controlSlot, irqState);
}

exception_t
Arch_decodeIRQControlInvocation(word_t invLabel, word_t length, cte_t *srcSlot, extra_caps_t excaps, word_t *buffer)
{
    word_t index, depth;
    cte_t *destSlot;
    cap_t cnodeCap;
    lookupSlot_ret_t lu_ret;
    exception_t status;
    word_t irq;
    word_t vector;

    if (!config_set(CONFIG_IRQ_IOAPIC)) {
        userError("IRQControl: Illegal operation.");
        current_syscall_error.type = seL4_IllegalOperation;
        return EXCEPTION_SYSCALL_ERROR;
    }

    /* check the common parameters */

    if (length < 7 || excaps.excaprefs[0] == NULL) {
        userError("IRQControl: Truncated message");
        current_syscall_error.type = seL4_TruncatedMessage;
        return EXCEPTION_SYSCALL_ERROR;
    }
    index = getSyscallArg(0, buffer);
    depth = getSyscallArg(1, buffer);
    cnodeCap = excaps.excaprefs[0]->cap;
    irq = getSyscallArg(6, buffer);
    if (irq > irq_user_max - irq_user_min) {
        userError("IRQControl: Invalid irq %ld should be between 0-%ld", (long)irq, (long)(irq_user_max - irq_user_min - 1));
        current_syscall_error.type = seL4_RangeError;
        current_syscall_error.rangeErrorMin = 0;
        current_syscall_error.rangeErrorMax = irq_user_max - irq_user_min;
        return EXCEPTION_SYSCALL_ERROR;
    }
    irq += irq_user_min;
    vector = irq + IRQ_INT_OFFSET;

    lu_ret = lookupTargetSlot(cnodeCap, index, depth);
    if (lu_ret.status != EXCEPTION_NONE) {
        return lu_ret.status;
    }

    destSlot = lu_ret.slot;

    status = ensureEmptySlot(destSlot);
    if (status != EXCEPTION_NONE) {
        return status;
    }

    switch (invLabel) {
    case IA32IRQIssueIRQHandlerIOAPIC: {
        word_t ioapic = getSyscallArg(2, buffer);
        word_t pin = getSyscallArg(3, buffer);
        word_t level = getSyscallArg(4, buffer);
        word_t polarity = getSyscallArg(5, buffer);


        if (isIRQActive(irq)) {
            current_syscall_error.type = seL4_RevokeFirst;
            return EXCEPTION_SYSCALL_ERROR;
        }

        status = ioapic_decode_map_pin_to_vector(ioapic, pin, level, polarity, vector);
        if (status != EXCEPTION_NONE) {
            return status;
        }

        setThreadState(ksCurThread, ThreadState_Restart);
        return invokeIssueIRQHandlerIOAPIC(irq, ioapic, pin, level, polarity, vector, destSlot, srcSlot);
    }
    break;
    case IA32IRQIssueIRQHandlerMSI: {
        word_t pci_bus = getSyscallArg(2, buffer);
        word_t pci_dev = getSyscallArg(3, buffer);
        word_t pci_func = getSyscallArg(4, buffer);
        word_t handle = getSyscallArg(5, buffer);
        x86_irq_state_t irqState;
        /* until we support msi interrupt remaping through vt-d we ignore the
         * vector and trust the user */
        (void)vector;
        if (isIRQActive(irq)) {
            current_syscall_error.type = seL4_RevokeFirst;
            return EXCEPTION_SYSCALL_ERROR;
        }

        if (pci_bus > PCI_BUS_MAX) {
            current_syscall_error.type = seL4_RangeError;
            current_syscall_error.rangeErrorMin = 0;
            current_syscall_error.rangeErrorMax = PCI_BUS_MAX;
            return EXCEPTION_SYSCALL_ERROR;
        }

        if (pci_dev > PCI_DEV_MAX) {
            current_syscall_error.type = seL4_RangeError;
            current_syscall_error.rangeErrorMin = 0;
            current_syscall_error.rangeErrorMax = PCI_DEV_MAX;
            return EXCEPTION_SYSCALL_ERROR;
        }

        if (pci_func > PCI_FUNC_MAX) {
            current_syscall_error.type = seL4_RangeError;
            current_syscall_error.rangeErrorMin = 0;
            current_syscall_error.rangeErrorMax = PCI_FUNC_MAX;
            return EXCEPTION_SYSCALL_ERROR;
        }

        irqState = x86_irq_state_irq_msi_new(pci_bus, pci_dev, pci_func, handle);

        setThreadState(ksCurThread, ThreadState_Restart);
        return Arch_invokeIRQControl(irq, destSlot, srcSlot, irqState);
    }
    break;
    default:
        userError("IRQControl: Illegal operation.");
        current_syscall_error.type = seL4_IllegalOperation;
        return EXCEPTION_SYSCALL_ERROR;
    }
}
