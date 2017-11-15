/*
 * Copyright 2017, Data61
 * Commonwealth Scientific and Industrial Research Organisation (CSIRO)
 * ABN 41 687 119 230.
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(DATA61_GPL)
 */

#ifndef __ARCH_MACHINE_GENERIC_TIMER_H_
#define __ARCH_MACHINE_GENERIC_TIMER_H_

#include <config.h>
#include <arch/machine/timer.h>
#include <mode/machine.h>
#include <mode/machine/timer.h>

/* ARM generic timer implementation */

static inline void
resetGenericTimer(void)
{
    SYSTEM_WRITE_WORD(CNT_TVAL, TIMER_RELOAD);
    SYSTEM_WRITE_WORD(CNT_CTL, BIT(0));
}

BOOT_CODE static inline void
initGenericTimer(void)
{
    if (config_set(CONFIG_DEBUG_BUILD)) {
        /* check the frequency is correct */
        uint32_t gpt_cnt_tval = 0;
        SYSTEM_READ_WORD(CNTFRQ, gpt_cnt_tval);
        if (gpt_cnt_tval != 0 && gpt_cnt_tval != TIMER_CLOCK_HZ) {
            printf("Warning:  gpt_cnt_tval %u, expected %u\n", gpt_cnt_tval,
                   (uint32_t) TIMER_CLOCK_HZ);
        }
    }

    resetGenericTimer();
}

#endif /* __ARCH_MACHINE_GENERIC_TIMER_H_ */
