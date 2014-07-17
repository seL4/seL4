/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(GD_GPL)
 */

#include <machine/assembler.h>

.code 32
.section .text, "ax"

.global lockTLBEntry

BEGIN_FUNC(lockTLBEntry)
    /* Set the lockdown register's preserve bit. */
    mrc p15, 0, r1, c10, c0, 0
    orr r1, r1, #1
    mcr p15, 0, r1, c10, c0, 0

    /* Flush any existing entry in the TLB. Note that this assumes that
     * there is no page boundary here (i.e., that the next two instruction
     * accesses don't cause TLB misses unless they are in the page being
     * locked); given that the kernel has a 16MB mapping for about 16k of
     * code, this is a reasonably safe assumption.
     */
    mcr p15, 0, r0, c8, c7, 1
    /* Now cause a TLB miss, to load the entry into the TLB locked region. */
    ldr r2, [r0]
    
    /* Clear the lockdown register's preserve bit. */
    mrc p15, 0, r1, c10, c0, 0
    bic r1, r1, #1
    mcr p15, 0, r1, c10, c0, 0
    
    bx lr
END_FUNC(lockTLBEntry)
