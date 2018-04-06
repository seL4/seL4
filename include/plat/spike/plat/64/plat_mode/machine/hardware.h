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

#if CONFIG_PT_LEVELS == 3
/* The top half of the address space is reserved for the kernel. This means that 256 top level
 * entires are for the user, and 256 are for the kernel. This will be further split into the
 * 'regular' kernel window, which contains mappings to physical memory, and a small higher
 * kernel image window that we use for running the actual kernel from */
#define PPTR_BASE        0xFFFFFFC000000000lu
/* We steal the top 2 gb entries for the kernel, this means that between PPTR_BASE and
 * KERNEL_BASE there are 254 entries remaining, which represents how much physical memory
 * can be used */
#define KERNEL_BASE      0xFFFFFFFF80000000lu
#else
#error Only PT_LEVELS == 3 is supported
#endif

#endif
