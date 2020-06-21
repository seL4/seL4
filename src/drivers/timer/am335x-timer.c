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

#define TIOCP_CFG_SOFTRESET BIT(0)

#define TIER_MATCH_ENABLE BIT(0)
#define TIER_OVERFLOW_ENABLE BIT(1)
#define TIER_COMPARE_ENABLE BIT(2)

#define TCLR_STARTTIMER BIT(0)
#define TCLR_AUTORELOAD BIT(1)
#define TCLR_PRESCALE_ENABLE BIT(5)
#define TCLR_COMPAREENABLE BIT(6)

timer_t *timer = (timer_t *) TIMER_PPTR;

#define WDT_REG(base, off) ((volatile uint32_t *)((base) + (off)))
#define WDT_REG_WWPS 0x34
#define WDT_REG_WSPR 0x48
#define WDT_WWPS_PEND_WSPR BIT(4)

#define SET_REGISTER(reg, mask) \
        do {                    \
            reg |= mask;        \
        } while((reg & mask) != mask);

static BOOT_CODE void disableWatchdog(void)
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
static BOOT_CODE void enableTimers(void)
{
    uint32_t cmper = CMPER_PPTR;

    /* XXX repeat this for DMTIMER4..7 */
    /* select clock - Timer 3 */
    *CMPER_REG(cmper, CMPER_CLKSEL_TIMER3) = CMPER_CKLSEL_MOSC;
    while ((*CMPER_REG(cmper, CMPER_CLKSEL_TIMER3) & RESERVED) != CMPER_CKLSEL_MOSC) {
        continue;
    }

    /* enable clock */
    *CMPER_REG(cmper, CMPER_TIMER3_CLKCTRL) = CMPER_CLKCTRL_ENABLE;
    while ((*CMPER_REG(cmper, CMPER_TIMER3_CLKCTRL) & RESERVED) != CMPER_CLKCTRL_ENABLE) {
        continue;
    }

    /* select clock - Timer 4 */
    *CMPER_REG(cmper, CMPER_CLKSEL_TIMER4) = CMPER_CKLSEL_MOSC;
    while ((*CMPER_REG(cmper, CMPER_CLKSEL_TIMER4) & RESERVED) != CMPER_CKLSEL_MOSC) {
        continue;
    }

    /* enable clock */
    *CMPER_REG(cmper, CMPER_TIMER4_CLKCTRL) = CMPER_CLKCTRL_ENABLE;
    while ((*CMPER_REG(cmper, CMPER_TIMER4_CLKCTRL) & RESERVED) != CMPER_CLKCTRL_ENABLE) {
        continue;
    }
}
#ifdef CONFIG_KERNEL_MCS

/* The idea here is to use the functionality of overflow interrupts to calculate time
   and use match interrupts for setting a deadline. */

uint32_t high_bits = 0;
BOOT_CODE void initTimer(void)
{
    int timeout;
    disableWatchdog();
    enableTimers();

    /* Configure dmtimer0 as kernel timer */
    SET_REGISTER(timer->cfg, TIOCP_CFG_SOFTRESET);

    /* disable */
    SET_REGISTER(timer->tclr, 0u);

    /* wait for reset */
    for (timeout = 10000; (timer->cfg & TIOCP_CFG_SOFTRESET) && timeout > 0; timeout--);

    if (!timeout) {
        printf("init timer failed\n");
        return;
    }

    maskInterrupt(/*disable*/ true, KERNEL_TIMER_IRQ);

    /* Set the reload value */
    SET_REGISTER(timer->tldr, 0u);

    /* Enables interrupt on overflow and match */
    SET_REGISTER(timer->tier, (TIER_OVERFLOW_ENABLE | TIER_MATCH_ENABLE));

    /* Clear the read register */
    SET_REGISTER(timer->tcrr, 0u);

    /* start the timer */
    SET_REGISTER(timer->tclr, (TCLR_AUTORELOAD | TCLR_STARTTIMER | TCLR_COMPAREENABLE));
}

#else /* CONFIG_KERNEL_MCS */


/* Configure dmtimer0 as kernel preemption timer */
BOOT_CODE void initTimer(void)
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

    maskInterrupt(/*disable*/ true, KERNEL_TIMER_IRQ);

    /* Set the reload value */
    timer->tldr = 0xFFFFFFFFUL - TIMER_RELOAD;

    /* Enables interrupt on overflow */
    timer->tier = TIER_OVERFLOW_ENABLE;

    /* Clear the read register */
    timer->tcrr = 0xFFFFFFFFUL - TIMER_RELOAD;

    /* Set autoreload and start the timer */
    timer->tclr = TCLR_AUTORELOAD | TCLR_STARTTIMER;
}
#endif /* CONFIG_KERNEL_MCS */
