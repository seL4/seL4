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

#ifndef __PLAT_MACHINE_H
#define __PLAT_MACHINE_H

enum IRQConstants {
    LIC_START_INTERRUPT             = 32,
    LOCAL_SPI_START_INTERRUPT       = LIC_START_INTERRUPT + 288,
    LOCAL_SPI_END_INTERRUPT         = LOCAL_SPI_START_INTERRUPT + 64 - 1,
    maxIRQ                          = LOCAL_SPI_END_INTERRUPT
} platform_interrupt_t;

compile_assert(interuppt_mapping_check, (maxIRQ == (32 + 288 + 64 - 1)))

#include <arch/machine/gic_pl390.h>

#endif  /* ! __PLAT_MACHINE_H */
