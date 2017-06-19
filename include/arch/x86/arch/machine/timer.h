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
#ifndef __ARCH_MACHINE_TIMER_H
#define __ARCH_MACHINE_TIMER_H

#include <config.h>
#include <arch/model/statedata.h>
#include <plat/machine.h>

BOOT_CODE uint32_t tsc_init(void);

#endif /* __ARCH_MACHINE_TIMER_H */
