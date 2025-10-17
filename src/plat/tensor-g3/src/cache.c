/*
 * Copyright 2025, Millpex
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

/*
 * Google Tensor G3 (Zuma) - L2/L3 Cache Configuration
 *
 * The SoC appears to use a variant of the ARM Cortex-A72 CPU complex.
 * Source: dmesg.txt identifies the CPU ID as [0x411fd461], which
 * corresponds to an ARM Cortex-A72 core (ARMv8-A).
 *
 * Cache hierarchy is expected to be:
 * - L1 I/D caches per core
 * - L2 cache per cluster (or shared)
 * - System-level L3 cache (shared)
 *
 * With ARMv8, cache coherency is managed by the hardware (e.g., via a
 * DynamIQ Shared Unit or similar). Therefore, standard ARM cache
 * maintenance operations (to Point of Coherency) are sufficient.
 */

#include <config.h>
#include <types.h>
#include <plat/machine.h>
#include <arch/machine/cache.h>
#include <object/tcb.h>
#include <utils/fence.h>

void plat_cleanL2Range(paddr_t start, paddr_t end)
{
    /* Use generic ARM cache clean operation. It operates to the PoC. */
    cleanCacheRange_RAM(start, end);
}

void plat_invalidateL2Range(paddr_t start, paddr_t end)
{
    /* Use generic ARM cache invalidate operation. */
    invalidateCacheRange_RAM(start, end);
}

void plat_cleanInvalidateL2Range(paddr_t start, paddr_t end)
{
    /* Use generic ARM clean and invalidate operation. */
    cleanInvalidateCacheRange_RAM(start, end);
}
