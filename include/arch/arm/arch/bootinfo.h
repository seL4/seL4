/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#pragma once

#ifdef CONFIG_ARCH_AARCH32

/* Modifiers:
 *  + 1: for the ASID area
 *  + 1: allow the kernel to release its own boot data region
 *  + 1: possible gap between ELF images and rootserver objects;
 *       see arm/arch_init_freemem */
#define MAX_NUM_FREEMEM_REG (ARRAY_SIZE(avail_p_regs) + 1 + 1 + 1)

/* kernel image, usage image, DTB from bootloader, ASID area */
#define NUM_RESERVED_REGIONS 4

#else /* all other ARM architectures (AARCH64)  */

/* This value is basically an arbitrary choice. We could calculate the exact
 * number, but just picking 16 will also do.
 */
#define MAX_NUM_FREEMEM_REG 16

/* kernel image, usage image, DTB from bootloader */
#define NUM_RESERVED_REGIONS 3

#endif


/* The maximum number of reserved regions is:
 * - 1 for each physical memory region (MAX_NUM_FREEMEM_REG)
 * - 1 for each kernel frame (NUM_KERNEL_DEVICE_FRAMES, there might be none)
 * - 1 for each reserved region (NUM_RESERVED_REGIONS)
 */
#define MAX_NUM_RESV_REG (MAX_NUM_FREEMEM_REG + NUM_KERNEL_DEVICE_FRAMES + \
                          NUM_RESERVED_REGIONS)
