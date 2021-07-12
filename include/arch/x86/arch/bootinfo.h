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

/* reserve region for user image */
#define NUM_RESERVED_REGIONS 1

/* The maximum number of reserved regions is:
 * - 1 for each physical memory region (MAX_NUM_FREEMEM_REG)
 * - 1 for each kernel region
 *     - ioapics (CONFIG_MAX_NUM_IOAPIC)
 *     - iommus (MAX_NUM_DRHU)
 *     - apic (1)
 * - 1 for each reserved region (NUM_RESERVED_REGIONS)
 * - 1 for the reserved MSI region
 */
#define MAX_NUM_RESV_REG (MAX_NUM_FREEMEM_REG + CONFIG_MAX_NUM_IOAPIC +
                          MAX_NUM_DRHU + 1 + NUM_RESERVED_REGIONS + 1)
