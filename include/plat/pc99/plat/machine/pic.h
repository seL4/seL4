/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#pragma once

#include <types.h>
#include <plat/machine.h>

/* Even if not using the PIC as the main interrupt controller we still
 * need to interact with it to remap and disable it */
void pic_remap_irqs(interrupt_t vector);
void pic_disable(void);

void pic_mask_irq(bool_t mask, irq_t irq);
bool_t pic_is_irq_pending(void);
void pic_ack_active_irq(void);

