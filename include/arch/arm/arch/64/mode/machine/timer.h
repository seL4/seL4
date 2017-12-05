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

#ifndef __ARCH_MODE_MACHINE_TIMER_H_
#define __ARCH_MODE_MACHINE_TIMER_H_

#include <config.h>
#include <stdint.h>
#include <util.h>

#ifdef CONFIG_ARM_HYPERVISOR_SUPPORT
#define CNT_TVAL "cnthp_tval_el2"
#define CNT_CTL  "cnthp_ctl_el2"
#else
#define CNT_TVAL "cntv_tval_el0"
#define CNT_CVAL "cntv_cval_el0"
#define CNT_CTL  "cntv_ctl_el0"
#endif
#define CNTFRQ   "cntfrq_el0"
#define CNT_CT   "cntvct_el0"

/* timer function definitions that work for all 64 bit arm platforms */
static inline CONST ticks_t
getMaxTicksToUs(void)
{
    return UINT64_MAX;
}

static inline CONST time_t
ticksToUs(ticks_t ticks)
{
    if (USE_KHZ) {
        return ticks / TIMER_CLOCK_KHZ / KHZ_IN_MHZ;
    } else {
        return ticks / TIMER_CLOCK_MHZ;
    }
}
#endif /*  __ARCH_MODE_MACHINE_TIMER_H_ */
