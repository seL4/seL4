/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#pragma once

#include <basic_types.h>



/**
 * irq_t is an identifier that represents a hardware interrupt.
 * irq handler capabilities refer to an irq_t which is then used by the
 * kernel to track irq state. An irq_t is also used to interface with an
 * interrupt controller driver using the functions below.
 * For most configurations an irq_t is a word_t type and the irq_t values
 * directly map to harware irq numbers and are also used as indexes into the
 * kernel's irq cnode that it uses for tracking state.
 * However on SMP configurations where there can be multiple irq_t identifiers
 * for a single hardware irq number, such as when there are core local interrupts,
 * irq_t cannot be assumed to be only a hardware irq number.
 * In this case, irq_t can be defined as a struct containing additional information.
 *
 * Macros are provided to hide this structural difference across configurations:
 * CORE_IRQ_TO_IRQT: converts from a core id and hw irq number to an irq_t
 * IRQT_TO_IDX: converts an irq_t to an index in the irq cnode. It is also used
 *   to encode the irq_t as a single word_t type for sending over IPIs.
 * IDX_TO_IRQT: converts an index in the irq cnode to an irq_t
 * IRQT_TO_CORE: extracts the core out of an irq_t
 * IRQT_TO_IRQL extracts a hw irq out of an irq_t.
 *
 * It is expected that interrupt controller drivers that support SMP provide
 * implementations of these Macros.
 * Currently only Arm SMP configurations use this scheme.
 */
#if defined(ENABLE_SMP_SUPPORT) && defined(CONFIG_ARCH_ARM)
typedef struct {
    word_t irq;
    word_t target_core;
} irq_t;
#else
typedef word_t irq_t;
#define CORE_IRQ_TO_IRQT(tgt, irq) (irq)
#define IRQT_TO_IDX(irq) (irq)
#define IDX_TO_IRQT(idx) (idx)
#define IRQT_TO_CORE(irqt) 0
#define IRQT_TO_IRQ(irqt) (irqt)
#endif

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

