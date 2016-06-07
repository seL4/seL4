/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(GD_GPL)
 */

#ifndef __MACHINE_HARDWARE_H
#define __MACHINE_HARDWARE_H

#include <types.h>
#include <arch/machine/hardware.h>
#include <plat/machine/hardware.h>
#include <plat/machine.h>

void handleReservedIRQ(irq_t irq);
void handleSpuriousIRQ(void);

/** MODIFIES: [*] */
void ackInterrupt(irq_t irq);

/** MODIFIES: [*] */
irq_t getActiveIRQ(void);

/** MODIFIES: [*] */
bool_t isIRQPending(void);

/** MODIFIES: [*] */
void maskInterrupt(bool_t enable, irq_t irq);
#endif
