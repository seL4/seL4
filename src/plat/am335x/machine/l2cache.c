/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(GD_GPL)
 */

#include <arch/machine.h>

static inline word_t
readACR(void)
{
    word_t ACR;
    asm volatile ("mrc p15,0,%0,c1,c0,1" : "=r"(ACR));
    return ACR;
}

static inline void
writeACR(word_t ACR)
{
    asm volatile ("mcr p15,0,%0,c1,c0,1" : : "r"(ACR));
}

void
initL2Cache(void)
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
