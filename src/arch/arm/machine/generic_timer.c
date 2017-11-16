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

#include <plat/machine/timer.h>
#include <arch/machine/generic_timer.h>

BOOT_CODE void initGenericTimer(void)
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

    resetTimer();
}
