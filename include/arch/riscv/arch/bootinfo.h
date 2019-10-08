/*
 * Copyright 2019, Data61
 * Commonwealth Scientific and Industrial Research Organisation (CSIRO)
 * ABN 41 687 119 230.
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(DATA61_GPL)
 */

#ifndef __ARCH_BOOTINFO_H
#define __ARCH_BOOTINFO_H

#define MAX_NUM_FREEMEM_REG 16

/* The maximum number of reserved regions is:
 * - 1 for each physical memory region (MAX_NUM_FREEMEM_REG)
 * - 1 for each kernel device (ARRAY_SIZE(kernel_devices))
 * - 1 for each mode-reserved region. (MODE_RESERVED)
 * - 1 each for kernel, dtb, and user image. (3)
 */
#ifndef CONFIG_PLAT_SPIKE
#define MAX_NUM_RESV_REG (MAX_NUM_FREEMEM_REG + ARRAY_SIZE(kernel_devices) + MODE_RESERVED + 3)
#else
/* spike has no devices, and ARRAY_SIZE(NULL) is invalid. */
#define MAX_NUM_RESV_REG (MAX_NUM_FREEMEM_REG + MODE_RESERVED + 3)
#endif

#endif
