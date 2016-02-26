/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(GD_GPL)
 */

/*
 * ARM Generic Interrupt Controller PL-390
 */


#ifndef __ARCH_MACHINE_GICPL390_H
#define __ARCH_MACHINE_GICPL390_H

#include <stdint.h>

typedef uint16_t interrupt_t;
typedef uint16_t irq_t;



/** MODIFIES: [*] */
interrupt_t getActiveIRQ(void);
/** MODIFIES: [*] */
interrupt_t getPendingIRQ(void);
/** MODIFIES: [*] */
bool_t isIRQPending(void);
/** MODIFIES: [*] */
void maskInterrupt(bool_t disable, interrupt_t irq);
/** MODIFIES: [*] */
void ackInterrupt(irq_t irq);

/** MODIFIES: [*] */
void initIRQController(void);

void handleSpuriousIRQ(void);

#endif /* !__ARCH_MACHINE_GICPL390_H */
