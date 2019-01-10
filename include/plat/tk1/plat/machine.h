/*
 * Copyright 2016, General Dynamics C4 Systems
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(GD_GPL)
 */

#ifndef __PLAT_MACHINE_H
#define __PLAT_MACHINE_H

#include <config.h>

enum IRQConstants {
    maxIRQ                          = 191
} platform_interrupt_t;

#include <arch/machine/gic_pl390.h>

#endif  /* ! __PLAT_MACHINE_H */
