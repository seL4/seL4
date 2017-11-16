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

/* ARM generic timer implementation */

static inline void
resetTimer(void)
{
    SYSTEM_WRITE_WORD(CNT_TVAL, TIMER_RELOAD);
    SYSTEM_WRITE_WORD(CNT_CTL, BIT(0));
}

BOOT_CODE void initGenericTimer(void);

#endif /* __ARCH_MACHINE_GENERIC_TIMER_H_ */
