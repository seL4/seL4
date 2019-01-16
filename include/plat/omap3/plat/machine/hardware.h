/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(GD_GPL)
 */

#ifndef __PLAT_MACHINE_HARDWARE_H
#define __PLAT_MACHINE_HARDWARE_H

#include <basic_types.h>
#include <linker.h>
#include <armv/machine.h>
#include <plat/machine.h>
#include <plat/machine/devices.h>
#include <plat/machine/devices_gen.h>
#include <plat/machine/interrupt.h>

static const kernel_frame_t BOOT_RODATA kernel_devices[] = {
    {
        /*  GP Timer 9 */
        GPTIMER9_PADDR,
        GPTIMER9_PPTR,
        true  /* armExecuteNever */
    },
    {
        /*  INTC */
        INTC_PADDR,
        INTC_PPTR,
        true  /* armExecuteNever */
#ifdef CONFIG_PRINTING
    },
    {
        /*  UART */
        UART3_PADDR,
        UART_PPTR,
        true  /* armExecuteNever */
#endif
    }
};

#endif
