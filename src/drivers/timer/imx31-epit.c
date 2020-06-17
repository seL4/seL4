/*
 * Copyright 2014, General Dynamics C4 Systems
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
#include <drivers/timer/imx31-epit_gen.h>

#define L2_LINE_SIZE_BITS 5
#define L2_LINE_SIZE BIT(L2_LINE_SIZE_BITS)

#define L2_LINE_START(a) ROUND_DOWN(a, L2_LINE_SIZE_BITS)
#define L2_LINE_INDEX(a) (L2_LINE_START(a)>>L2_LINE_SIZE_BITS)

timer_t *epit1 = (timer_t *) EPIT_PPTR;

enum IPGConstants {
    IPG_CLK = 1,
    IPG_CLK_HIGHFREQ = 2,
    IPG_CLK_32K = 3
};

#define TIMER_CLOCK_SRC   IPG_CLK_32K

/* Configure EPIT1 as kernel preemption timer */
BOOT_CODE void initTimer(void)
{
    epitcr_t epitcr_kludge;

    /* Stop timer */
    epit1->epitcr = 0;

    /* Configure timer */
    epitcr_kludge.words[0] = 0; /* Zero struct */
    epitcr_kludge = epitcr_set_clksrc(epitcr_kludge, TIMER_CLOCK_SRC);
    /* Overwrite counter immediately on write */
    epitcr_kludge = epitcr_set_iovw(epitcr_kludge, 1);
    /* Reload from modulus register */
    epitcr_kludge = epitcr_set_rld(epitcr_kludge, 1);
    /* Enable interrupt */
    epitcr_kludge = epitcr_set_ocien(epitcr_kludge, 1);
    /* Count from modulus value on restart */
    epitcr_kludge = epitcr_set_enmod(epitcr_kludge, 1);
    epit1->epitcr = epitcr_kludge.words[0];

    /* Set counter modulus */
    epit1->epitlr = TIMER_RELOAD;

    /* Interrupt at zero count */
    epit1->epitcmpr = 0;

    /* Clear pending interrupt */
    epit1->epitsr = 1;

    /* Enable timer */
    epitcr_kludge = epitcr_set_en(epitcr_kludge, 1);
    epit1->epitcr = epitcr_kludge.words[0];
}

