/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(GD_GPL)
 */

#ifndef __ARCH_MACHINE_HARDWARE_H
#define __ARCH_MACHINE_HARDWARE_H

#include <config.h>
#include <mode/machine/hardware.h>

/* PPTR_TOP is shared between all ARM platforms. */

#ifdef CONFIG_BENCHMARK_USE_KERNEL_LOG_BUFFER
#define PPTR_TOP 0xffe00000
#define KS_LOG_PPTR PPTR_TOP
#else
#define PPTR_TOP 0xfff00000
#endif /* CONFIG_BENCHMARK_USE_KERNEL_LOG_BUFFER */

#endif /* !__ARCH_MACHINE_HARDWARE_H */
