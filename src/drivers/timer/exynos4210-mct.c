/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#include <config.h>
#include <types.h>
#include <machine/io.h>
#include <kernel/vspace.h>
#include <arch/machine.h>
#include <arch/kernel/vspace.h>
#include <plat/machine.h>
#include <linker.h>
#include <plat/machine/devices_gen.h>
#include <plat/machine/hardware.h>
#include <drivers/timer/arm_generic.h>
#include <drivers/timer/mct.h>

timer_t *mct = (timer_t *) EXYNOS_MCT_PPTR;

BOOT_CODE void initTimer(void)
{
    mct_clear_write_status();

    /* use the arm generic timer, backed by the mct */
    /* enable the timer */
    mct->global.tcon = GTCON_EN;
    while (mct->global.wstat != GWSTAT_TCON);
    mct->global.wstat = GWSTAT_TCON;

    initGenericTimer();
}

