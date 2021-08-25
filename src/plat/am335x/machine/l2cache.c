/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#include <arch/machine.h>

static inline word_t readACR(void)
{
    word_t ACR;
    asm volatile("mrc p15,0,%0,c1,c0,1" : "=r"(ACR));
    return ACR;
}

static inline void writeACR(word_t ACR)
{
    asm volatile("mcr p15,0,%0,c1,c0,1" : : "r"(ACR));
}

void initL2Cache(void)
{
    cleanInvalidateL1Caches();

    /*
     * Set the L2EN bit in the Auxially Control Register.
     *
     * We assume the C bit is already set in the system control register (from
     * head.S), and that the L2 Cache Auxilliary Control Register is correct
     * (as per reset).
     */
    writeACR(readACR() | 0x2);

    cleanInvalidateL1Caches();
}

void plat_cleanL2Range(paddr_t start, paddr_t end) {}
void plat_invalidateL2Range(paddr_t start, paddr_t end) {}
void plat_cleanInvalidateL2Range(paddr_t start, paddr_t end) {}
void plat_cleanInvalidateL2Cache(void) {}
