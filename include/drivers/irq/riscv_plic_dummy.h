/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 * Copyright 2021, HENSOLDT Cyber
 *
 * SPDX-License-Identifier: GPL-2.0-only
 *
 * A dummy PLIC driver for platforms like spike that do not implement a PLIC.
 * This can be taken as a template for platform specific PLIC drivers.
 */

#pragma once

/* Since this is a template, it has a dummy implementation for the trigger API
 * function plic_irq_set_trigger().
 */
#define HAVE_SET_TRIGGER 1

#include <arch/machine/plic.h>

static inline irq_t plic_get_claim(void)
{
    printf("no PLIC present, can't claim any interrupt\n");
    return irqInvalid;
}

static inline void plic_complete_claim(irq_t irq)
{
    printf("no PLIC present, can't complete claim for interrupt %d\n",
           (int)irq);
}

static inline void plic_mask_irq(bool_t disable, irq_t irq)
{
    printf("no PLIC present, can't %s interrupt %d\n",
           disable ? "mask" : "unmask", (int)irq);
}

static inline void plic_irq_set_trigger(irq_t irq, bool_t edge_triggered)
{
    printf("no PLIC present, can't set interrupt %d to %s triggered\n",
           (int)irq, edge_triggered ? "edge" : "level");
}

static inline void plic_init_hart(void)
{
    printf("no PLIC present, skip hart specific initialisation\n");
}

static inline void plic_init_controller(void)
{
    printf("no PLIC present, skip platform specific initialisation\n");
}
