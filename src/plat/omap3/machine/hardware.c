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
#include <arch/linker.h>
#include <plat/machine/devices.h>
#include <plat/machine/timer.h>

#define INTCPS_SYSCONFIG_SOFTRESET BIT(1)
#define INTCPS_SYSSTATUS_RESETDONE BIT(0)

#define TIER_OVERFLOW BIT(1u)
#define TIER_MATCH    BIT(0u)

#define TIOCP_CFG_SOFTRESET BIT(1u)
#define TCLR_AUTORELOAD     BIT(1u)
#define TCLR_COMPAREENABLE  BIT(6u)
#define TCLR_STARTTIMER     BIT(0u)

/**
   DONT_TRANSLATE
 */
BOOT_CODE void
initTimer(void)
{
    /* Configure gptimer9 as kernel timer */

    /* disable */
    timer->tclr = 0;

    /* perform a soft reset */
    timer->cfg = TIOCP_CFG_SOFTRESET;

    /* wait for reset */
    while (!timer->tistat);

    maskInterrupt(/*disable*/ true, GPT9_IRQ);

    /* Set the reload value */
    timer->tldr = 0u;

    /* Enables interrupt on overflow and match */
    timer->tier |= (TIER_OVERFLOW | TIER_MATCH);

    /* Clear the read register */
    timer->tcrr = 0u;

    /* start the timer */
    timer->tclr = TCLR_AUTORELOAD | TCLR_STARTTIMER | TCLR_COMPAREENABLE;
}

/**
   DONT_TRANSLATE
 */
BOOT_CODE void
initIRQController(void)
{
    intc->intcps_sysconfig = INTCPS_SYSCONFIG_SOFTRESET;
    while (!(intc->intcps_sysstatus & INTCPS_SYSSTATUS_RESETDONE)) ;
}

void plat_cleanL2Range(paddr_t start, paddr_t end) {}
void plat_invalidateL2Range(paddr_t start, paddr_t end) {}
void plat_cleanInvalidateL2Range(paddr_t start, paddr_t end) {}
