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

/* Modifiers:
 *  + 1: allow the kernel to release its own boot data region
 *  + 1: possible gap between ELF images and rootserver objects;
 *       see arm/arch_init_freemem */
#define MAX_NUM_FREEMEM_REG (ARRAY_SIZE(avail_p_regs) + MODE_RESERVED + 1 + 1)

/* The maximum number of reserved regions is:
 * - 1 for each physical memory region (MAX_NUM_FREEMEM_REG)
 * - 1 for each kernel device (ARRAY_SIZE(kernel_devices))
 * - 1 for each mode-reserved region. (MODE_RESERVED)
 * - 1 each for kernel, dtb, and user image. (3)
 */
#define MAX_NUM_RESV_REG (MAX_NUM_FREEMEM_REG + ARRAY_SIZE(kernel_devices) + MODE_RESERVED + 3)

#endif
