/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#pragma once

#include <config.h>
#include <arch/model/statedata.h>
#include <plat/machine.h>

#ifdef CONFIG_KERNEL_MCS
#include <mode/util.h>
#include <arch/kernel/xapic.h>

static inline CONST time_t getKernelWcetUs(void)
{
    return  10u;
}

static inline PURE ticks_t usToTicks(time_t us)
{
    assert(x86KStscMhz > 0);
    return us * x86KStscMhz;
}

static inline PURE time_t getMaxUsToTicks(void)
{
    return div64(UINT64_MAX, x86KStscMhz);
}

static inline PURE ticks_t getTimerPrecision(void)
{
    return usToTicks(1u);
}

static inline void ackDeadlineIRQ(void)
{
}

static inline ticks_t getCurrentTime(void)
{
    return x86_rdtsc();
}

static inline CONST ticks_t getMaxTicksToUs(void)
{
    return UINT64_MAX;
}

static inline PURE time_t ticksToUs(ticks_t ticks)
{
    return div64(ticks, x86KStscMhz);
}

static inline void setDeadline(ticks_t deadline)
{
    assert(deadline > NODE_STATE(ksCurTime));
    if (likely(x86KSapicRatio == 0)) {
        x86_wrmsr(IA32_TSC_DEADLINE_MSR, deadline);
    } else {
        /* convert deadline from tscKhz to apic khz */
        deadline -= getCurrentTime();
        apic_write_reg(APIC_TIMER_COUNT, div64(deadline, x86KSapicRatio));
    }
}
#else
static inline void resetTimer(void)
{
    /* nothing to do */
}
#endif /* CONFIG_KERNEL_MCS */


BOOT_CODE uint32_t tsc_init(void);

