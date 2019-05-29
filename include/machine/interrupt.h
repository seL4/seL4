/*
 * Copyright 2018, Data61
 * Commonwealth Scientific and Industrial Research Organisation (CSIRO)
 * ABN 41 687 119 230.
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(DATA61_GPL)
 */

#ifndef __MACHINE_INTERRUPT_H
#define __MACHINE_INTERRUPT_H

#include <basic_types.h>

typedef word_t irq_t;

/**
 * Return a currently pending IRQ.
 *
 * This function can be called multiple times and needs to return the same IRQ
 * until ackInterrupt is called. getActiveIRQ returns irqInvalid if no interrupt
 * is pending. It is assumed that if isIRQPending is true, then getActiveIRQ
 * will not return irqInvalid. irqInvalid is a per platform constant that cannot
 * correspond to an actual IRQ raised by the platform.
 *
 * @return     The active IRQ. irqInvalid if no IRQ is pending.
 */
static inline irq_t getActiveIRQ(void);

/**
 * Checks if an IRQ is currently pending in the hardware.
 *
 * isIRQPending is used to determine whether to preempt long running operations
 * at various preemption points throughout the kernel. If this returns true, it
 * means that if the Kernel were to return to user mode, it would then
 * immediately take an interrupt.
 *
 * @return     True if irq pending, False otherwise.
 */
static inline bool_t isIRQPending(void);

/**
 * maskInterrupt disables and enables IRQs.
 *
 * When an IRQ is disabled, it should not raise an interrupt on the processor.
 *
 * @param[in]  disable  True to disable IRQ, False to enable IRQ
 * @param[in]  irq      The irq to modify
 */
static inline void maskInterrupt(bool_t disable, irq_t irq);

/**
 * Acks the interrupt
 *
 * ackInterrupt is used by the kernel to indicate it has processed the interrupt
 * delivery and getActiveIRQ is now able to return a different IRQ number. Note
 * that this is called after a notification has been signalled to user level,
 * but before user level has handled the cause and does not imply that the cause
 * of the interrupt has been handled.
 *
 * @param[in]  irq   irq to ack
 */
static inline void ackInterrupt(irq_t irq);

/**
 * Called when getActiveIRQ returns irqInvalid while the kernel is handling an
 * interrupt entry. An implementation is not required to do anything here, but
 * can report the spurious IRQ or try prevent it from reoccuring.
 */
static inline void handleSpuriousIRQ(void);

/**
 * Handle a platform-reserved IRQ.
 *
 * Platform specific implementation for handling IRQs for interrupts that are
 * reserved and not made available to user-level. Will be called if getActiveIRQ
 * returns an IRQ number that is reserved. After this function returns,
 * ackInterrupt will likely be immediately called after.
 *
 * @param[in]  irq   The irq
 */
static inline void handleReservedIRQ(irq_t irq);

#endif /* __MACHINE_INTERRUPT_H */
