/*
 * Copyright 2025, Millpex
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#include <config.h>
#include <types.h>
#include <plat/timer.h>
#include <arch/machine/generic_timer.h>

/*
 * Initialize the timer for Google Tensor G3 (Zuma)
 *
 * The Tensor G3 uses the ARM Generic Timer.
 * Source (Frequency): test_clk.txt confirms a 24.576MHz clock source.
 * Source (IRQ): dmesg.txt confirms the architected timer uses IRQ 30.
 */
void initTimer(void)
{
    /*
     * TODO: Verify if the 24.576MHz clock source for the generic timer
     * needs to be manually un-gated via the SoC's clock controller registers.
     * This implementation ASSUMES the bootloader has already enabled it.
     */

    /*
     * Initialize the ARM Generic Timer. This function reads the frequency
     * from CNTFRQ_EL0 (set by bootloader) and sets up the per-core timer.
     */
    initGenericTimer();
}
