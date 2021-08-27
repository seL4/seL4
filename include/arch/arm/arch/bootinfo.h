/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#pragma once

#ifdef CONFIG_ARCH_AARCH32

/* The number of required free memory regions is:
 * +1 for each available free physical memory region (entries in avail_p_regs)
 * +1: for the ASID area
 * +1 to allow the kernel to release its own boot data region
 * +1 for a possible gap between ELF images and rootserver objects
 */
#define MAX_NUM_FREEMEM_REG (ARRAY_SIZE(avail_p_regs) + 1 + 1 + 1)

/* The number of reserved regions is:
 * +1 for each kernel frame (NUM_KERNEL_DEVICE_FRAMES, there might be none)
 * +1 for the ASID area
 * +1 for the kernel image
 * +1 for the usage image
 * +1 for the DTB from bootloader
 */
#define NUM_RESERVED_REGIONS (NUM_KERNEL_DEVICE_FRAMES + 1 + 1 + 1 + 1)

#else /* all other ARM architectures (AARCH64)  */

/* This value is basically an arbitrary choice. We could calculate the exact
 * number, but just picking 16 will also do fine. This is part of the memory
 * is used during kernel boot only, it can be made available later.
 */
#define MAX_NUM_FREEMEM_REG 16

/* The number of reserved regions is:
 * +1 for each kernel frame (NUM_KERNEL_DEVICE_FRAMES, there might be none)
 * +1 for the kernel image
 * +1 for the usage image
 * +1 for the DTB from bootloader
 */
#define NUM_RESERVED_REGIONS (NUM_KERNEL_DEVICE_FRAMES + 1 + 1 + 1)


#endif
