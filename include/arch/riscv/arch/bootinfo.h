/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#pragma once

#define MAX_NUM_FREEMEM_REG 16

/* The maximum number of reserved regions is:
 * - 1 for each physical memory region (MAX_NUM_FREEMEM_REG)
 * - 1 for each kernel region (NUM_KERNEL_DEVICE_FRAMES, there might be none)
 * - 1 for each mode-reserved region. (MODE_RESERVED)
 * - 1 each for kernel, dtb, and user image. (3)
 */
#define MAX_NUM_RESV_REG (MAX_NUM_FREEMEM_REG + NUM_KERNEL_DEVICE_FRAMES + \
                          MODE_RESERVED + 3)
