/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#include <config.h>
#include <machine/assembler.h>

#if defined(CONFIG_ARM_CORTEX_A15) || defined(CONFIG_ARM_CORTEX_A7)
  /* The hardware does not support tlb locking */

#else

.code 32
.section .text, "ax"
.global lockTLBEntryCritical

.balign (16*4)    
BEGIN_FUNC(lockTLBEntryCritical)
 
 /* lockTLBEntryCritical should lie within a single page so that spurious TLB walks do
 * not interfere. Aligning to a 64-byte instruction boundary will suffice, as
 * the critical section (i.e. this function) fits within 64 bytes.
 */
    /* Invalidate both I & D TLB entry */
    mcr p15, 0, r0, c8, c7, 1

    /* Now lock it! */
    mcr p15, 0, r1, c10, c0, 1 /* Start locking walked I-TLB entries. */
    mcr p15, 0, r0, c10, c1, 1 /* Load I-TLB entry. */
    mcr p15, 0, r2, c10, c0, 1 /* Finish locking walked I-TLB entries. */

    mcr p15, 0, r1, c10, c0, 0 /* Start locking walked D-TLB entries. */
    mcr p15, 0, r0, c10, c1, 0 /* Load D-TLB entry. */
    mcr p15, 0, r2, c10, c0, 0 /* Finish locking walked D-TLB entries. */

    bx lr
END_FUNC(lockTLBEntryCritical)

#endif /* ARM_CORTEX_A15 */
