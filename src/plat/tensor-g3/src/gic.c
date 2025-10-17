/*
 * Copyright 2025, Millpex
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#include <config.h>
#include <types.h>
#include <plat/gic.h>
#include <arch/machine/gic_v3.h>
#include <utils/io.h>

/*
 * Initializes the GICv3 Distributor. This is done once by the primary core.
 * Source (Hardware): dmesg.txt confirms a GICv3 is present.
 */
void plat_gic_init(void)
{
    /*
     * The GIC driver in seL4 (`gic_v3.c`) takes the distributor base address
     * and handles the full initialization sequence.
     */
    gic_v3_dist_init(GICD_BASE);
}

/*
 * Initializes the per-core GIC components (Redistributor and CPU Interface).
 */
void plat_interrupt_controller_init(void)
{
    /*
     * The seL4 driver handles initializing the per-CPU interfaces. It needs
     * the base of the redistributor region for this core.
     */
    gic_v3_cpu_interface_init(GICR_BASE);
}
