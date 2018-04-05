/*
 * Copyright 2018, Data61
 * Commonwealth Scientific and Industrial Research Organisation (CSIRO)
 * ABN 41 687 119 230.
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(DATA61_GPL)
 */
#ifndef __PLAT_MODE_MACHINE_HARDWARE_H
#define __PLAT_MODE_MACHINE_HARDWARE_H

#include <config.h>

#define physBase          0x00000000C0000000llu

#if CONFIG_PT_LEVELS == 2
#define kernelBase        0xffffffff80000000llu
#elif CONFIG_PT_LEVELS == 3
#define kernelBase        0xFFFFFFC000000000llu
#elif CONFIG_PT_LEVELS == 4
#define kernelBase        0xFFFF800000000000llu
#endif /* CONFIG_PT_LEVELS */

#ifdef CONFIG_ROCKET_CHIP
#define PPTR_TOP         0xFFFFFFFF8FE00000llu
#else
#define PPTR_TOP         0x000000004FC00000llu
#endif

#endif
