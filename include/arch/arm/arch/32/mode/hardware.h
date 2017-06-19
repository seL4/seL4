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

#ifndef __ARCH_HARDWARE_32_H
#define __ARCH_HARDWARE_32_H

#include <config.h>
#include <arch/machine/hardware.h>
#include <plat/machine/hardware.h>

/*
 * 0xffe00000 asid id slot (arm/arch/kernel/vspace.h)
 * 0xfff00000 devices      (plat/machine/devices.h)
 * 0xffff0000 vectors      (arch/machine/hardware.h)
 * 0xffffc000 global page  (arch/machine/hardware.h)
 */
#define BASE_OFFSET (kernelBase - physBase)
#ifdef CONFIG_BENCHMARK_USE_KERNEL_LOG_BUFFER
#define PPTR_TOP 0xffe00000
#define KS_LOG_PPTR PPTR_TOP
#else
#define PPTR_TOP 0xfff00000
#endif /* CONFIG_BENCHMARK_USE_KERNEL_LOG_BUFFER */
#define PADDR_TOP (PPTR_TOP - BASE_OFFSET)

#endif /* __ARCH_HARDWARE_32_H */
