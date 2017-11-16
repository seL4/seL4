/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(GD_GPL)
 */

#include <config.h>
#include <types.h>
#include <machine/io.h>
#include <kernel/vspace.h>
#include <arch/machine.h>
#include <arch/kernel/vspace.h>
#include <plat/machine.h>
#include <linker.h>
#include <plat/machine/devices.h>
#include <plat/machine/hardware.h>
#include <plat/machine/timer.h>
#include <plat/machine/hardware_gen.h>
#include <arch/benchmark_overflowHandler.h>

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

interrupt_t active_irq = irqInvalid;

/* Configure EPIT1 as kernel preemption timer */
BOOT_CODE void
initTimer(void)
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

static void cleanL2(void)
{
    /* clean all ways */
    imx31_l2cc_flush_regs->clean_by_way = 0xff;
    /* Busy-wait for completion */
    while (imx31_l2cc_flush_regs->clean_by_way);
}

static void invalidateL2(void)
{
    /* Invalidate all ways. */
    imx31_l2cc_flush_regs->inv_by_way = 0xff;
    /* Busy-wait for completion. */
    while (imx31_l2cc_flush_regs->inv_by_way);
}

static void finaliseL2Op(void)
{
    /* We sync the l2 cache, which drains the write and eviction
       buffers, to ensure that everything is consistent with RAM. */
    imx31_l2cc_flush_regs->sync = 1;
}

void plat_cleanL2Range(paddr_t start, paddr_t end)
{
    paddr_t line;
    word_t index;

    for (index = L2_LINE_INDEX(start);
            index < L2_LINE_INDEX(end) + 1;
            index++) {
        line = index << L2_LINE_SIZE_BITS;
        imx31_l2cc_flush_regs->clean_by_pa = line;
    }
    finaliseL2Op();
}

void plat_invalidateL2Range(paddr_t start, paddr_t end)
{
    paddr_t line;
    word_t index;

    for (index = L2_LINE_INDEX(start);
            index < L2_LINE_INDEX(end) + 1;
            index++) {
        line = index << L2_LINE_SIZE_BITS;
        imx31_l2cc_flush_regs->inv_by_pa = line;
    }

    finaliseL2Op();
}

void plat_cleanInvalidateL2Range(paddr_t start, paddr_t end)
{
    paddr_t line;
    word_t index;

    for (index = L2_LINE_INDEX(start);
            index < L2_LINE_INDEX(end) + 1;
            index++) {
        line = index << L2_LINE_SIZE_BITS;
        imx31_l2cc_flush_regs->clinv_by_pa = line;
    }
    finaliseL2Op();
}

void plat_cleanInvalidateCache(void)
{
    cleanL2();
    invalidateL2();
}

BOOT_CODE void
initL2Cache(void)
{
#ifndef CONFIG_DEBUG_DISABLE_L2_CACHE
    /* Configure L2 cache */
    imx31_l2cc_ctrl_regs->aux_control = 0x0003001b;

    /* Invalidate the L2 cache */
    invalidateL2();

    /* Enable the L2 cache */
    imx31_l2cc_ctrl_regs->control = 1;
#endif
}

BOOT_CODE void
initIRQController(void)
{
    /* Do nothing */
}

BOOT_CODE void cpu_initLocalIRQController(void)
{
    /* Do nothing */
}
