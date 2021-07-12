/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#pragma once

/* This value is basically an arbitrary choice. We could calculate the exact
 * number, but just picking 16 will also do.
 */
#define MAX_NUM_FREEMEM_REG 16

/* The maximum number of reserved regions is:
 * - 1 for each physical memory region (MAX_NUM_FREEMEM_REG)
 * - 1 for each kernel region (NUM_KERNEL_DEVICE_FRAMES, there might be none)
 * - 1 each for kernel, dtb, and user image. (3)
 */
#define MAX_NUM_RESV_REG (MAX_NUM_FREEMEM_REG + NUM_KERNEL_DEVICE_FRAMES + 3)
