/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#pragma once

#define PLIC_MAX_NUM_INT   0
#define IRQ_CNODE_SLOT_BITS 1

static inline irq_t plic_get_claim(void)
{
    return irqInvalid;
}

static inline void plic_complete_claim(irq_t irq)
{
}

static inline void plic_mask_irq(bool_t disable, irq_t irq)
{
}

static inline void plic_init_controller(void)
{

}

/* Available physical memory regions on platform (RAM minus kernel image). */
/* NOTE: Regions are not allowed to be adjacent! */
static p_region_t BOOT_DATA avail_p_regs[] = {
    /* The first 2MB are reserved for the SBI in the BBL */
    { /*.start = */ 0x0, /* .end = */ 0x10000000}
};

static const paddr_t BOOT_RODATA *kernel_devices = NULL;

static const p_region_t BOOT_RODATA *dev_p_regs = NULL;
