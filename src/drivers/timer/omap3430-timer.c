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
#include <plat/machine.h>
#include <linker.h>
#include <plat/machine/devices_gen.h>
#include <plat/machine/hardware.h>

#define TIOCP_CFG_SOFTRESET BIT(1)
#define TCLR_AUTORELOAD     BIT(1)
#define TCLR_COMPAREENABLE  BIT(6)
#define TCLR_STARTTIMER     BIT(0)
#define TIER_MATCHENABLE    BIT(0)
#define TIER_OVERFLOWENABLE BIT(1)

timer_t *timer = (timer_t *) TIMER_PPTR;

#ifdef CONFIG_KERNEL_MCS
#define INTCPS_SYSCONFIG_SOFTRESET BIT(1)
#define INTCPS_SYSSTATUS_RESETDONE BIT(0)
uint32_t high_bits = 0;
BOOT_CODE void initTimer(void)
{
    /* Configure gptimer9 as kernel timer */
    timer->cfg = TIOCP_CFG_SOFTRESET;

    /* disable */
    timer->tclr = 0;

    /* wait for reset */
    while (!timer->tistat);

    maskInterrupt(/*disable*/ true, KERNEL_TIMER_IRQ);

    /* Set the reload value */
    timer->tldr = 0u;

    /* Enables interrupt on overflow and match */
    timer->tier |= (TIER_OVERFLOWENABLE | TIER_MATCHENABLE);

    /* Clear the read register */
    timer->tcrr = 0u;

    /* start the timer */
    timer->tclr = TCLR_AUTORELOAD | TCLR_STARTTIMER | TCLR_COMPAREENABLE;
}
#else /* CONFIG_KERNEL_MCS */
#define TIMER_INTERVAL_MS (CONFIG_TIMER_TICK_MS)
BOOT_CODE void initTimer(void)
{
    /* Configure gptimer9 as kernel timer */
    timer->cfg = TIOCP_CFG_SOFTRESET;

    while (!timer->tistat);

    maskInterrupt(/*disable*/ true, KERNEL_TIMER_IRQ);

    /* Set the reload value */
    timer->tldr = 0xFFFFFFFFUL - TIMER_RELOAD;

    /* Enables interrupt on overflow */
    timer->tier = TIER_OVERFLOWENABLE;

    /* Clear the read register */
    timer->tcrr = 0xFFFFFFFFUL - TIMER_RELOAD;

    /* Set autoreload and start the timer */
    timer->tclr = TCLR_AUTORELOAD | TCLR_STARTTIMER;
}
#endif /* !CONFIG_KERNEL_MCS */
