/*
 * Copyright 2019, Data61
 * Commonwealth Scientific and Industrial Research Organisation (CSIRO)
 * ABN 41 687 119 230.
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(DATA61_GPL)
 */

#ifndef __DRIVER_IRQ_HIFIVE_H
#define __DRIVER_IRQ_HIFIVE_H

#include <plat/machine/devices_gen.h>

#define PLIC_PPTR_BASE       PLIC_PPTR + 0x0C000000

#define PLIC_HARTID (CONFIG_FIRST_HART_ID * 2)

#define PLIC_PRIO            0x0
#define PLIC_PRIO_PER_ID     0x4

#define PLIC_EN         0x2000
#define PLIC_EN_PER_HART    0x80

#define PLIC_THRES      0x200000
#define PLIC_THRES_PER_HART 0x1000
#define PLIC_THRES_CLAIM    0x4

typedef uint32_t interrupt_t;
static inline uint32_t readl(const volatile uint64_t addr)
{
    uint32_t val;
    asm volatile("lw %0, 0(%1)" : "=r"(val) : "r"(addr));

    return val;
}

static inline void writel(uint32_t val, volatile uint64_t addr)
{
    asm volatile("sw %0, 0(%1)" : : "r"(val), "r"(addr));
}


static inline interrupt_t plic_get_claim(void)
{
    /* Read the claim register for our HART interrupt context */
    return readl(PLIC_PPTR_BASE + PLIC_THRES + PLIC_THRES_PER_HART * PLIC_HARTID +
                 PLIC_THRES_CLAIM);
}

static inline void plic_complete_claim(interrupt_t irq)
{
    /* Complete the IRQ claim by writing back to the claim register. */
    writel(irq, PLIC_PPTR_BASE + PLIC_THRES + PLIC_THRES_PER_HART * PLIC_HARTID +
           PLIC_THRES_CLAIM);
}

static inline void plic_mask_irq(bool_t disable, interrupt_t irq)
{
    uint64_t addr = 0;
    uint32_t val = 0;

    if (irq >= 32) {
        irq -= 32;
        addr = 0x4;
    }

    addr += PLIC_PPTR_BASE + PLIC_EN;
    val = readl(addr + PLIC_EN_PER_HART * PLIC_HARTID);
    if (disable) {
        val &= ~BIT(irq);
    } else {
        val |= BIT(irq);
    }

    writel(val, addr + PLIC_EN_PER_HART * PLIC_HARTID);

    if (!disable) {
        /* Clear any pending claims if we are enabling otherwise they may not
         * be raised again */
        plic_complete_claim(irq);
    }
}

static inline void plic_init_controller(void)
{
    uint32_t pending;

    /* Clear all pending bits */
    pending = readl(PLIC_PPTR_BASE + 0x1000);
    for (int i = 0; i < 32 ; i++) {
        if (pending & (1 << i)) {
            readl(PLIC_PPTR_BASE + PLIC_THRES +
                  PLIC_THRES_PER_HART * PLIC_HARTID +
                  PLIC_THRES_CLAIM);
        }
    }
    pending = readl(PLIC_PPTR_BASE + 0x1004);
    for (int i = 0; i < 22 ; i++) {
        if (pending & (1 << i)) {
            readl(PLIC_PPTR_BASE + PLIC_THRES +
                  PLIC_THRES_PER_HART * PLIC_HARTID +
                  PLIC_THRES_CLAIM);
        }
    }

    /* Disable interrupts */
    writel(0, PLIC_PPTR_BASE + PLIC_EN + PLIC_EN_PER_HART * PLIC_HARTID);
    writel(0, PLIC_PPTR_BASE + PLIC_EN + PLIC_EN_PER_HART * PLIC_HARTID + 0x4);

    /* Set threshold to zero */
    writel(1, (PLIC_PPTR_BASE + PLIC_THRES + PLIC_THRES_PER_HART * PLIC_HARTID));

    /* Set the priorities of all interrupts to 1 */
    for (int i = 1; i <= PLIC_MAX_IRQ + 1; i++) {
        writel(2, PLIC_PPTR_BASE + PLIC_PRIO + PLIC_PRIO_PER_ID * i);
    }

}

#endif /* __DRIVER_IRQ_HIFIVE_H */
