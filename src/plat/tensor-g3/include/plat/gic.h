/*
 * Copyright 2025, Millpex
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#pragma once

/*
 * Base addresses for the GICv3 interrupt controller on Tensor G3.
 *
 * TODO: These addresses are placeholders. You must find the correct
 * physical addresses from the SoC Technical Reference Manual (TRM).
 */
#define GICD_BASE    0x10A00000  /* Placeholder for GIC Distributor */
#define GICR_BASE    0x10A40000  /* Placeholder for GIC Redistributor */

/*
 * Initialize the GIC Distributor (common to all cores).
 * Must be called by the primary core before other cores are brought up.
 */
void plat_gic_init(void);

/*
 * Initialize the GIC Redistributor and CPU Interface for the calling core.
 * This must be called on each core.
 */
void plat_interrupt_controller_init(void);

