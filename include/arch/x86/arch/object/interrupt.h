/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(GD_GPL)
 */

#ifndef __ARCH_OBJECT_INTERRUPT_H
#define __ARCH_OBJECT_INTERRUPT_H

#include <types.h>
#include <api/failures.h>
#include <object/structures.h>
#include <plat/machine.h>

exception_t Arch_decodeIRQControlInvocation(word_t label, word_t length,
                                            cte_t *srcSlot, extra_caps_t extraCaps,
                                            word_t *buffer);
void Arch_irqStateInit(void);
void Arch_updateIRQState(word_t irq, x86_irq_state_t state);
exception_t Arch_checkIRQ(word_t irq);
void Arch_maskInterrupt(bool_t disable, irq_t irq);
void setInterruptMode(irq_t irq, bool_t levelTrigger, bool_t polarityLow);

#endif
