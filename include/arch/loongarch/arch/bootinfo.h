/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#pragma once

#include <config.h>
#include <plat/machine/devices_gen.h>

/* The value for the max number of free memory region is basically an arbitrary
 * choice. We could calculate the exact number, but just picking 16 will also
 * do for now. Increase this value if the boot fails.
 */
#define MAX_NUM_FREEMEM_REG 16

/* The regions reserved by the boot code are:
 * +1 for kernel
 * +1 for device tree binary
 * +1 for user image.
 */
#define NUM_RESERVED_REGIONS 3

/* The maximum number of reserved regions is:
 * +1 for each free memory region (MAX_NUM_FREEMEM_REG)
 * +1 for each kernel frame (NUM_KERNEL_DEVICE_FRAMES, there might be none)
 * +1 for each region reserved by the boot code (NUM_RESERVED_REGIONS)
 */
#define MAX_NUM_RESV_REG (MAX_NUM_FREEMEM_REG + NUM_KERNEL_DEVICE_FRAMES + \
                          NUM_RESERVED_REGIONS)
