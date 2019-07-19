/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(GD_GPL)
 */

#ifndef __OBJECT_INTERRUPT_H
#define __OBJECT_INTERRUPT_H

#include <types.h>
#include <api/failures.h>
#include <object/structures.h>
#include <arch/object/interrupt.h>
#include <plat/machine.h>

#if ! (defined CORE_IRQ_TO_IDX && defined IDX_TO_IRQ && defined IDX_TO_CORE && defined IRQ_IS_PPI)
/* The following macros are used to translate a target core and a hardware irq
 * to their internal seL4 representation. Some IRQs are core specific so their
 * number should be duplicated as many times as there are cores. seL4 stores
 * all IRQs in a flat array hence the need for translating (core,irq) into the
 * array index. These macros should be defined at the interrupt controller
 * level.  Here we fallback on a "flat" translation (ie we assume one core or
 * no private interrupt).
 */
#define CORE_IRQ_TO_IDX(tgt, irq) ((irq_t) (irq))
#define IDX_TO_IRQ(idx) (idx)
#define IDX_TO_CORE(idx) 0
#define IRQ_IS_PPI(irq) false
#endif

exception_t decodeIRQControlInvocation(word_t invLabel, word_t length,
                                       cte_t *srcSlot, extra_caps_t excaps,
                                       word_t *buffer);
exception_t invokeIRQControl(irq_t irq, cte_t *handlerSlot, cte_t *controlSlot);
exception_t decodeIRQHandlerInvocation(word_t invLabel, irq_t irq,
                                       extra_caps_t excaps);
void invokeIRQHandler_AckIRQ(irq_t irq);
void invokeIRQHandler_SetIRQHandler(irq_t irq, cap_t cap, cte_t *slot);
void invokeIRQHandler_ClearIRQHandler(irq_t irq);
void deletingIRQHandler(irq_t irq);
void deletedIRQHandler(irq_t irq);
void handleInterrupt(irq_t irq);
bool_t isIRQActive(irq_t irq);
void setIRQState(irq_state_t irqState, irq_t irq);

#endif
