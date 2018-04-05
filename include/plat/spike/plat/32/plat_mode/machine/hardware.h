/*
 * Copyright 2018, Data61
 * Commonwealth Scientific and Industrial Research Organisation (CSIRO)
 * ABN 41 687 119 230.
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(DATA61_GPL)
 */
#ifndef __PLAT_MODE_MACHINE_HARDWARE_H
#define __PLAT_MODE_MACHINE_HARDWARE_H

#include <config.h>

#define physBase          0xC0000000lu
#define kernelBase        0x80000000lu

#ifdef CONFIG_ROCKET_CHIP
#define PPTR_TOP          0x8FE00000lu
#else
#define PPTR_TOP          0x4FC00000lu
#endif

#endif
