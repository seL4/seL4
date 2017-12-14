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

timer_t *gpt = (timer_t *) GPT_PPTR;
ticks_t high_bits = 0;

enum IPGConstants {
    IPG_CLK = 1,
    IPG_CLK_HIGHFREQ = 2,
    IPG_CLK_32K = 3
};

/* gptcr bits */
#define EN     0
#define ENMOD  1
#define FRR    9
#define CLKSRC 6
#define SWR    15

interrupt_t active_irq = irqInvalid;

/* Configure GPT as kernel preemption timer */
BOOT_CODE void
initTimer(void)
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
