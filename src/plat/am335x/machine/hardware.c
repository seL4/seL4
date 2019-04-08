/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(GD_GPL)
 */

#include <config.h>
#include <types.h>
#include <plat/machine/hardware.h>

BOOT_CODE void initIRQController(void)
{
    intc->intcps_sysconfig = INTCPS_SYSCONFIG_SOFTRESET;
    while (!(intc->intcps_sysstatus & INTCPS_SYSSTATUS_RESETDONE)) ;
}

BOOT_CODE void cpu_initLocalIRQController(void) {}
