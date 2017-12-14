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

#include <config.h>
#include <types.h>
#include <machine/io.h>
#include <kernel/vspace.h>
#include <arch/machine.h>
#include <arch/kernel/vspace.h>
#include <linker.h>

/* gptcr bits */
#define EN     0
#define ENMOD  1
#define FRR    9
#define CLKSRC 6
#define SWR    15

timer_t *gpt = (timer_t *) TIMER_PPTR;
ticks_t high_bits = 0;

enum IPGConstants {
    IPG_CLK = 1,
    IPG_CLK_HIGHFREQ = 2,
    IPG_CLK_32K = 3
};

interrupt_t active_irq = irqInvalid;

BOOT_CODE void initTimer(void)
{
    /* reset the gpt */
    gpt->gptcr = 0;
    /* clear the status register */
    gpt->gptcr = 0x3F;
    /* software reset */
    gpt->gptcr = BIT(SWR);
    /* configure the gpt */
    gpt->gptcr = BIT(ENMOD) | BIT(FRR) | (IPG_CLK_HIGHFREQ << CLKSRC);
    /* enable overflow irq */
    gpt->gptir = BIT(ROV);
    /* turn it on */
    gpt->gptcr |= BIT(EN);
}
