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

#ifndef __DRIVERS_TIMER_ARM_GENERIC_H_
#define __DRIVERS_TIMER_ARM_GENERIC_H_

#include <config.h>
#include <mode/machine.h>

/* ARM generic timer implementation */
#ifdef CONFIG_KERNEL_MCS
#include <model/statedata.h>
#include <api/types.h>
/** DONT_TRANSLATE **/
static inline ticks_t getCurrentTime(void)
{
    ticks_t time;
    SYSTEM_READ_64(CNT_CT, time);
    return time;
}

/** DONT_TRANSLATE **/
static inline void setDeadline(ticks_t deadline)
{
    assert(deadline >= NODE_STATE(ksCurTime));
    SYSTEM_WRITE_64(CNT_CVAL, deadline);
}

static inline void ackDeadlineIRQ(void)
{
    ticks_t deadline = UINT64_MAX;
    setDeadline(deadline);
}
#else /* CONFIG_KERNEL_MCS */
#include <arch/machine/timer.h>
static inline void resetTimer(void)
{
    SYSTEM_WRITE_WORD(CNT_TVAL, TIMER_RELOAD);
    SYSTEM_WRITE_WORD(CNT_CTL, BIT(0));
}
#endif /* !CONFIG_KERNEL_MCS */

BOOT_CODE void initGenericTimer(void);

#ifdef CONFIG_ARM_HYPERVISOR_SUPPORT
static uint64_t read_cntpct(void) UNUSED;
static void save_virt_timer(vcpu_t *vcpu);
static void restore_virt_timer(vcpu_t *vcpu);
#endif /* CONFIG_ARM_HYPERVISOR_SUPPORT */

#endif /* __DRIVERS_TIMER_ARM_GENERIC_H_ */
