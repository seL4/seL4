/*
 * Copyright 2017, Data61
 * Commonwealth Scientific and Industrial Research Organisation (CSIRO)
 * ABN 41 687 119 230.
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(DATA61_GPL)
 */

#include <util.h>
#include <stdint.h>
#include <plat/machine.h>
BOOT_CODE void
initIRQController(void)
{
    /* Disable all interrupts */
    intc_regs->bfDisableIRQs[0] = 0xffffffff;
    intc_regs->bfDisableIRQs[1] = 0xffffffff;
    intc_regs->bfDisableBasicIRQs = 0xffffffff;
    /* Disable FIQ */
    intc_regs->FIQ_control &= ~FIQCTRL_FIQ_ENABLE;
    /* Enable IRQ control for GPU */
    intc_regs->bfEnableBasicIRQs = BIT(INTERRUPT_BASIC_IRQ_PENDING_REGISTER1 - BASIC_IRQ_OFFSET);
    intc_regs->bfEnableBasicIRQs = BIT(INTERRUPT_BASIC_IRQ_PENDING_REGISTER2 - BASIC_IRQ_OFFSET);

    core_regs->coreTimerPrescaler = 0x80000000;
}

BOOT_CODE void cpu_initLocalIRQController(void) {}

interrupt_t
getActiveIRQ(void)
{
    uint32_t pending;
    uint32_t irq;
    /* Read core interrupt register */
    pending = core_regs->coreIRQSource[0];
    /* Mask out invalid bits */
    pending &= MASK(12);
    /* If pending == 0 spurious interrupt */
    if (pending == 0) {
        return irqInvalid;
    }

    /* Get IRQ number */
    irq = (31 - clzl(pending));
    if (irq != INTERRUPT_CORE_GPU) {
        return irq;
    }

    /* GPU interrupt */
    pending = intc_regs->bfIRQBasicPending;
    pending &= intc_regs->bfEnableBasicIRQs;
    /* Mask out pending register 0 and 1 */
    pending &= ~BIT(INTERRUPT_BASIC_IRQ_PENDING_REGISTER1 - BASIC_IRQ_OFFSET);
    pending &= ~BIT(INTERRUPT_BASIC_IRQ_PENDING_REGISTER2 - BASIC_IRQ_OFFSET);
    if (pending) {
        return (31 - clzl(pending)) + BASIC_IRQ_OFFSET;
    }

    pending = intc_regs->bfGPUIRQPending[1];
    pending &= intc_regs->bfEnableIRQs[1];
    if (pending) {
        return (31 - clzl(pending)) + 32 + NORMAL_IRQ_OFFSET;
    }
    pending = intc_regs->bfGPUIRQPending[0];
    pending &= intc_regs->bfEnableIRQs[0];
    if (pending) {
        return (31 - clzl(pending)) + 0 + NORMAL_IRQ_OFFSET;
    }

    return irqInvalid;
}

void
maskInterrupt(bool_t disable, interrupt_t irq)
{
    switch (irq) {
    case INTERRUPT_CORE_CNTPSIRQ :
    case INTERRUPT_CORE_CNTPNSIRQ:
    case INTERRUPT_CORE_CNTHPIRQ :
    case INTERRUPT_CORE_CNTVIRQ  :
        if (disable) {
            core_regs->coreTimersIrqCtrl[0] &= ~BIT(irq);
        } else {
            core_regs->coreTimersIrqCtrl[0] |= BIT(irq);
        }
        return;
    case INTERRUPT_CORE_MAILBOX_0:
    case INTERRUPT_CORE_MAILBOX_1:
    case INTERRUPT_CORE_MAILBOX_2:
    case INTERRUPT_CORE_MAILBOX_3:
        if (disable) {
            core_regs->coreMailboxesIrqCtrl[0] &= ~BIT(irq);
        } else {
            core_regs->coreMailboxesIrqCtrl[0] |= BIT(irq);
        }
        return;
    case INTERRUPT_CORE_LOCAL_TIMER:
        if (disable) {
            core_regs->localTimerCtl &= ~BIT(LOCAL_TIMER_CTRL_IRQ_BIT);
        } else {
            core_regs->localTimerCtl |= BIT(LOCAL_TIMER_CTRL_IRQ_BIT);
        }
        return;
    case INTERRUPT_CORE_GPU:
    // Not maskable
    case INTERRUPT_CORE_PMU:
    // Not currently handled
    case INTERRUPT_CORE_AXI:
        // Not currently handled
        return;
    default:
        break;
    }
    if (irq < BASIC_IRQ_OFFSET) {
        // Other invalid irq
        return;
    }

    if (irq < NORMAL_IRQ_OFFSET) {
        if (disable) {
            intc_regs->bfDisableBasicIRQs = BIT(irq - BASIC_IRQ_OFFSET);
        } else {
            intc_regs->bfEnableBasicIRQs = BIT(irq - BASIC_IRQ_OFFSET);
        }
    } else if (irq < maxIRQ) {
        int normal_irq = irq - NORMAL_IRQ_OFFSET;
        int index = normal_irq / 32;
        if (disable) {
            intc_regs->bfDisableIRQs[index] = BIT(normal_irq % 32);
        } else {
            intc_regs->bfEnableIRQs[index] = BIT(normal_irq % 32);
        }
    }
}
