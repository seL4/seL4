/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#include <arch/machine/hardware.h>


void clean_D_PoU(void)
{
    /* V6 has a single op to clean D-Cache */
    asm volatile("mcr p15, 0, %0, c7, c10, 0" : : "r"(0));
}


void cleanInvalidate_D_PoC(void)
{
    /* V6 has a single op to clean+invalidate D-Cache */
    asm volatile("mcr p15, 0, %0, c7, c14, 0" : : "r"(0));
}

void cleanInvalidate_L1D(void)
{
    cleanInvalidate_D_PoC();
}
