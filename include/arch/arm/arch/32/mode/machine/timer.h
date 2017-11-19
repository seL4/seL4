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
#include <plat/machine/hardware.h>
#include <mode/machine.h>

/* Use Hypervisor Physical timer */
#ifdef CONFIG_ARM_HYPERVISOR_SUPPORT
#define CNT_TVAL CNTHP_TVAL
#define CNT_CTL  CNTHP_CTL
#define CNT_CVAL CNTHP_CVAL
#else
/* Use virtual timer */
#define CNT_TVAL CNTV_TVAL
#define CNT_CTL  CNTV_CTL
#define CNT_CVAL CNTV_CVAL
#endif

#endif /* __ARCH_MODE_MACHINE_TIMER_H_ */
