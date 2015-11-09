/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(GD_GPL)
 */

#ifndef __PLAT_MACHINE_PIC_H
#define __PLAT_MACHINE_PIC_H

#include <types.h>
#include <plat/machine.h>

/* Even if not using the PIC as the main interrupt controller we still
 * need to interact with it to remap and disable it */
void pic_remap_irqs(interrupt_t vector);
void pic_disable(void);

void pic_mask_irq(bool_t mask, irq_t irq);
bool_t pic_is_irq_pending(void);
void pic_ack_active_irq(void);

#endif
