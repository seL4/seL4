/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#pragma once

#define MAX_NUM_FREEMEM_REG 16

/*
 * The maximum number of reserved regions we have is:
 * - 1 for each physical memory region (MAX_NUM_FREEMEM_REG)
 * - 1 for each kernel device:
 *      - ioapics (CONFIG_MAX_NUM_IOAPIC)
 *      - iommus (MAX_NUM_DRHU)
 *      - apic (1)
 *      - the reserved MSI region (1)
 */
#define MAX_NUM_RESV_REG (MAX_NUM_FREEMEM_REG + CONFIG_MAX_NUM_IOAPIC + MAX_NUM_DRHU + 2)
