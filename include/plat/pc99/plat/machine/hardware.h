/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(GD_GPL)
 */

#ifndef __PLAT_MACHINE_HARDWARE_H
#define __PLAT_MACHINE_HARDWARE_H

#include <config.h>
#include <types.h>
#include <plat/machine.h>
#include <plat/machine/hardware_gen.h>

#if defined(X86_32)
#include <plat/machine/hardware_32.h>
#endif

void handleReservedIRQ(irq_t irq);
void ackInterrupt(irq_t irq);
irq_t getActiveIRQ(void);
bool_t isIRQPending(void);
void resetTimer(void);
void platAddDevices(void);

void handleSpuriousIRQ(void);

#endif
