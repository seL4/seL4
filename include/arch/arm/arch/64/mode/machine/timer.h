/*
 * Copyright 2016, Data61
 * Commonwealth Scientific and Industrial Research Organisation (CSIRO)
 * ABN 41 687 119 230.
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(DATA61_GPL)
 */

#ifndef __ARCH_MODE_MACHINE_TIMER_H_
#define __ARCH_MODE_MACHINE_TIMER_H_

#include <config.h>
#include <plat/machine/hardware.h>
#include <mode/machine.h>

#define HZ_IN_KHZ 1000llu
/* convert to khz first to avoid overflow */
#define TICKS_PER_MS (TIMER_CLOCK_HZ / HZ_IN_KHZ)
/* but multiply by timer tick ms */
#define TIMER_RELOAD    (TICKS_PER_MS * CONFIG_TIMER_TICK_MS)

/* ARM generic timer implementation */

static inline void
resetGenericTimer(void)
{
    MSR("cntv_tval_el0", TIMER_RELOAD);
    MSR("cntv_ctl_el0", BIT(0));
}

BOOT_CODE static inline void
initGenericTimer(void)
{
    if (config_set(CONFIG_DEBUG_BUILD)) {
        /* check the frequency is correct */
        uint32_t gpt_cnt_tval;
        MRS("cntfrq_el0", gpt_cnt_tval);
        if (gpt_cnt_tval != 0 && gpt_cnt_tval != TIMER_CLOCK_HZ) {
            printf("gpt_cnt_tval %u, expected %u\n", gpt_cnt_tval, (uint32_t) TIMER_CLOCK_HZ);
            /* it's ok to call halt here as it's a debug build */
            halt();
        }
    }

    resetGenericTimer();
}

#endif /*  __ARCH_MODE_MACHINE_TIMER_H_ */
