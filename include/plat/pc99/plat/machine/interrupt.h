/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#pragma once

#include <config.h>
#include <types.h>
#include <util.h>

#include <arch/object/structures.h>
#include <arch/model/statedata.h>
#include <arch/kernel/apic.h>
#include <machine/interrupt.h>
#include <plat/machine/acpi.h>
#include <plat/machine/ioapic.h>
#include <plat/machine/pic.h>
#include <plat/machine/intel-vtd.h>

static inline void handleReservedIRQ(irq_t irq)
{
#ifdef CONFIG_IOMMU
    if (irq == irq_iommu) {
        vtd_handle_fault();
        return;
    }
#endif

#ifdef CONFIG_IRQ_REPORTING
    printf("Received unhandled reserved IRQ: %d\n", (int)irq);
#endif
}

static inline void receivePendingIRQ(void)
{
    assert(ARCH_NODE_STATE(x86KSPendingInterrupt) == int_invalid);
    asm volatile("sti\n"
                 "nop\n"
                 "cli\n"
                 : "=m"(ARCH_NODE_STATE(x86KSPendingInterrupt)));
}

static inline interrupt_t servicePendingIRQ(void)
{
    assert(ARCH_NODE_STATE(x86KScurInterrupt) == int_invalid);
    assert(ARCH_NODE_STATE(x86KSPendingInterrupt) != int_invalid);
    interrupt_t ret = ARCH_NODE_STATE(x86KSPendingInterrupt);
    ARCH_NODE_STATE(x86KSPendingInterrupt) = int_invalid;
    return ret;
}

/* Get the IRQ number currently working on. */
static inline irq_t getActiveIRQ(void)
{
    if (ARCH_NODE_STATE(x86KScurInterrupt) == int_invalid) {
        /* If we tried to get the active IRQ when we don't have one then
         * we are polling for an interrupt for some reason, in which case
         * we should try to get a pending interrupt if there isn't already
         * one.
         * This logic is here and not in the main call sites in handleSyscall
         * because this is only relevant on some interrupt controllers (notably
         * the x86 APIC) and is cleaner to have here */
        if (ARCH_NODE_STATE(x86KSPendingInterrupt) == int_invalid) {
            receivePendingIRQ();
            /* Check if there was no pending IRQ */
            if (ARCH_NODE_STATE(x86KSPendingInterrupt) == int_invalid) {
                return irqInvalid;
            }
        }
        /* Prepare to handle pending IRQ */
        ARCH_NODE_STATE(x86KScurInterrupt) = servicePendingIRQ();
    }
    return ARCH_NODE_STATE(x86KScurInterrupt) - IRQ_INT_OFFSET;
}

/* Checks for pending IRQ */
static inline bool_t isIRQPending(void)
{
    if (apic_is_interrupt_pending()) {
        return true;
    }

    if (config_set(CONFIG_IRQ_PIC) && pic_is_irq_pending()) {
        return true;
    }

    return false;
}

static inline void ackInterrupt(irq_t irq)
{
    if (config_set(CONFIG_IRQ_PIC) && irq <= irq_isa_max) {
        pic_ack_active_irq();
    } else {
        apic_ack_active_interrupt();
    }

    ARCH_NODE_STATE(x86KScurInterrupt) = int_invalid;
}

static inline void handleSpuriousIRQ(void)
{
    /* do nothing */
}

static void inline updateIRQState(irq_t irq, x86_irq_state_t state)
{
    assert(irq <= maxIRQ);
    x86KSIRQState[irq] = state;
}

static inline void maskInterrupt(bool_t disable, irq_t irq)
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
            updateIRQState(irq, state);
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

