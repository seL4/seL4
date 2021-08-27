/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#pragma once

/* This value is basically an arbitrary choice. We could calculate the exact
 * number, but just picking 16 will also do fine. This is part of the memory
 * is used during kernel boot only, it can be made available later.
 */
#define MAX_NUM_FREEMEM_REG 16

/* The number of reserved regions is:
 * +1 for each kernel region (NUM_KERNEL_DEVICE_FRAMES, there might be none)
 * +1 for the kernel image
 * +1 for the usage image
 * +1 for the DTB from bootloader
 */
#define NUM_RESERVED_REGIONS (NUM_KERNEL_DEVICE_FRAMES + 1 + 1 + 1)
