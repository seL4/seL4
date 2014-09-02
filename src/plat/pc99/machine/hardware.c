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
#include <machine/io.h>
#include <arch/kernel/apic.h>
#include <arch/model/statedata.h>
#include <arch/linker.h>
#include <plat/machine/pic.h>
#include <plat/machine/ioapic.h>
#include <plat/machine.h>

#ifdef CONFIG_IOMMU
#include <plat/machine/intel-vtd.h>
#endif

/* ============================== interrupts/IRQs ============================== */

/* Enable or disable irq according to the 'mask' flag. */
void maskInterrupt(bool_t mask, irq_t irq)
{
    assert(irq >= irq_controller_min);
    assert(irq <= maxIRQ);

    if (irq <= irq_controller_max) {
#ifdef CONFIG_IRQ_IOAPIC
        ioapic_mask_irq(mask, irq);
#else
        pic_mask_irq(mask, irq);
#endif
    } else {
        /* we can't mask/unmask specific APIC vectors (e.g. MSIs/IPIs) */
    }
}

/* Set mode of an irq */
void setInterruptMode(irq_t irq, bool_t levelTrigger, bool_t polarityLow)
{
#ifdef CONFIG_IRQ_IOAPIC
    assert(irq >= irq_ioapic_min);
    assert(irq <= maxIRQ);

    if (irq <= irq_ioapic_max) {
        ioapic_set_mode(irq, levelTrigger, polarityLow);
    } else {
        /* No mode setting for specific APIC vectors */
    }
#endif
}

/* Handle a platform-reserved IRQ. */
void handleReservedIRQ(irq_t irq)
{
#ifdef CONFIG_IOMMU
    if (irq == irq_iommu) {
        vtd_handle_fault();
        return;
    }
#endif
    printf("Received reserved IRQ: %d\n", (int)irq);
}

/* Get the IRQ number currently working on. */
irq_t getActiveIRQ(void)
{
    if (ia32KScurInterrupt == int_invalid) {
        return irqInvalid;
    } else {
        return ia32KScurInterrupt - IRQ_INT_OFFSET;
    }
}

/* Checks for pending IRQ */
bool_t isIRQPending(void)
{
    if (apic_is_interrupt_pending()) {
        return true;
    }
#ifdef CONFIG_IRQ_PIC
    if (pic_is_irq_pending()) {
        return true;
    }
#endif
    return false;
}

void ackInterrupt(irq_t irq)
{
#ifdef CONFIG_IRQ_PIC
    if (irq <= irq_isa_max) {
        pic_ack_active_irq();
    } else
#endif
    {
        apic_ack_active_interrupt();
    }
}

void handleSpuriousIRQ(void)
{
    /* Do nothing */
}

/* ============================== timer ============================== */

void resetTimer(void)
{
    /* not necessary */
}
