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

#define INTCPS_SYSCONFIG_SOFTRESET BIT(1)
#define INTCPS_SYSSTATUS_RESETDONE BIT(0)

#define TIMER_INTERVAL_MS (CONFIG_TIMER_TICK_MS)

#define TIOCP_CFG_SOFTRESET BIT(1)
#define TCLR_AUTORELOAD     BIT(1)
#define TCLR_COMPAREENABLE  BIT(6)
#define TCLR_STARTTIMER     BIT(0)
#define TIER_MATCHENABLE    BIT(0)
#define TIER_OVERFLOWENABLE BIT(1)

timer_t *timer = (timer_t *) GPTIMER9_PPTR;

BOOT_CODE void
initTimer(void)
{
    /* Configure gptimer9 as kernel timer */
    timer->cfg = TIOCP_CFG_SOFTRESET;

    while (!timer->tistat);

    maskInterrupt(/*disable*/ true, GPT9_IRQ);

    /* Set the reload value */
    timer->tldr = 0xFFFFFFFFUL - TIMER_RELOAD;

    /* Enables interrupt on overflow */
    timer->tier = TIER_OVERFLOWENABLE;

    /* Clear the read register */
    timer->tcrr = 0xFFFFFFFFUL - TIMER_RELOAD;

    /* Set autoreload and start the timer */
    timer->tclr = TCLR_AUTORELOAD | TCLR_STARTTIMER;
}

BOOT_CODE void
initIRQController(void)
{
    intc->intcps_sysconfig = INTCPS_SYSCONFIG_SOFTRESET;
    while (!(intc->intcps_sysstatus & INTCPS_SYSSTATUS_RESETDONE)) ;
}

BOOT_CODE void cpu_initLocalIRQController(void) {}

void plat_cleanL2Range(paddr_t start, paddr_t end) {}
void plat_invalidateL2Range(paddr_t start, paddr_t end) {}
void plat_cleanInvalidateL2Range(paddr_t start, paddr_t end) {}
void plat_cleanInvalidateCache(void) {}
