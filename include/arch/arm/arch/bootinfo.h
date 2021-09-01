/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#pragma once

#include <config.h>
#include <plat/machine/devices_gen.h>
#include <kernel/vspace.h>

/* The max number of free memory regions is:
 * +1 for each available physical memory region (elements in avail_p_regs)
 * +1 for each MODE_RESERVED region, there might be none
 * +1 to allow the kernel to release its own boot data region
 * +1 for a possible gap between ELF images and rootserver objects
 */
#define MAX_NUM_FREEMEM_REG (ARRAY_SIZE(avail_p_regs) + MODE_RESERVED + 1 + 1)

/* The maximum number of reserved regions is:
 * +1 for each free memory region (MAX_NUM_FREEMEM_REG)
 * +1 for each kernel frame (NUM_KERNEL_DEVICE_FRAMES, there might be none)
 * +1 for each mode-reserved region (MODE_RESERVED)
 * +1 for each region reserved by the boot code (3: kernel, dtb, user image)
 */
#define MAX_NUM_RESV_REG (MAX_NUM_FREEMEM_REG + NUM_KERNEL_DEVICE_FRAMES + \
                          MODE_RESERVED + 3)
