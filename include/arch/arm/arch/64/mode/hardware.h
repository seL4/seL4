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

#ifndef __ARCH_MODE_HARDWARE_H
#define __ARCH_MODE_HARDWARE_H

#include <config.h>
#include <arch/machine/hardware.h>
#include <plat/machine/hardware.h>

#define BASE_OFFSET (kernelBase - physBase)

#ifdef CONFIG_ARM_HYPERVISOR_SUPPORT
#define PPTR_TOP 0xffffc0000000lu
#else
#define PPTR_TOP 0xffffffffc0000000
#endif

#define PADDR_TOP (PPTR_TOP - BASE_OFFSET)

#endif /* __ARCH_MODE_HARDWARE_H */
