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

#define TIOCP_CFG_SOFTRESET BIT(0)

#define TIER_MATCHENABLE BIT(0)
#define TIER_OVERFLOWENABLE BIT(1)
#define TIER_COMPAREENABLE BIT(2)

#define TCLR_AUTORELOAD BIT(1)
#define TCLR_COMPAREENABLE BIT(6)
#define TCLR_STARTTIMER BIT(0)

timer_t *timer = (timer_t *) DMTIMER0_PPTR;

#define WDT_REG(base, off) ((volatile uint32_t *)((base) + (off)))
#define WDT_REG_WWPS 0x34
#define WDT_REG_WSPR 0x48
#define WDT_WWPS_PEND_WSPR BIT(4)

static BOOT_CODE void
disableWatchdog(void)
{
    uint32_t wdt = WDT1_PPTR;

    // am335x ref man, sec 20.4.3.8
    *WDT_REG(wdt, WDT_REG_WSPR) = 0xaaaa;
    while ((*WDT_REG(wdt, WDT_REG_WWPS) & WDT_WWPS_PEND_WSPR)) {
        continue;
    }
    *WDT_REG(wdt, WDT_REG_WSPR) = 0x5555;
    while ((*WDT_REG(wdt, WDT_REG_WWPS) & WDT_WWPS_PEND_WSPR)) {
        continue;
    }
}

/*
 * Enable DMTIMER clocks, otherwise their registers wont be accessible.
 * This could be moved out of kernel.
 */
static BOOT_CODE void
enableTimers(void)
{
    uint32_t cmper = CMPER_PPTR;

    /* XXX repeat this for DMTIMER4..7 */
    /* select clock */
    *CMPER_REG(cmper, CMPER_CLKSEL_TIMER3) = CMPER_CKLSEL_MOSC;
    while ((*CMPER_REG(cmper, CMPER_CLKSEL_TIMER3) & 3) != CMPER_CKLSEL_MOSC) {
        continue;
    }

    /* enable clock */
    *CMPER_REG(cmper, CMPER_TIMER3_CLKCTRL) = CMPER_CLKCTRL_ENABLE;
    while ((*CMPER_REG(cmper, CMPER_TIMER3_CLKCTRL) & 3) != CMPER_CLKCTRL_ENABLE) {
        continue;
    }
}

/* Configure dmtimer0 as kernel preemption timer */
BOOT_CODE void
initTimer(void)
{
    int timeout;

    disableWatchdog();
    enableTimers();

    timer->cfg = TIOCP_CFG_SOFTRESET;

    for (timeout = 10000; (timer->cfg & TIOCP_CFG_SOFTRESET) && timeout > 0; timeout--)
        ;
    if (!timeout) {
        printf("init timer failed\n");
        return;
    }

    maskInterrupt(/*disable*/ true, DMTIMER0_IRQ);

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
