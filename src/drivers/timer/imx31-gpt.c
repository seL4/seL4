/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 *
 * SPDX-License-Identifier: GPL-2.0-only
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
