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

#if defined(ARM_CORTEX_A15)
/* A15 hardware does not support TLB locking */
BEGIN_FUNC(lockTLBEntry)
    bx lr
END_FUNC(lockTLBEntry)

#else /* !ARM_CORTEX_A15 */

/* lockTLBEntry should lie within a single page so that spurious TLB walks do
 * not interfere. Aligning to a 64-byte instruction boundary will suffice, as
 * the function critical section of the function fits within 64 bytes.
 */
    .balign (16*4)
BEGIN_FUNC(lockTLBEntry)
    /* Lock this entry into the next position.
     * On Cortex-A8, the first N TLB entries can be locked,
     * where N is specified in the TLB lockdown register.
     *
     * Find out how many entries are already locked.
     */
    ldr r3, tlb_lock_count

    /* Set up the values we need to program into the lockdown register. */
#if defined(ARM_CORTEX_A8)
    /* Before lockdown, base = victim = num_locked_tlb_entries. */
    mov r1, #1
    orr r1, r1, r3, lsl #27
    orr r1, r1, r3, lsl #22

    /* After lockdown, base = victim = num_locked_tlb_entries + 1. */
    add r3, r3, #1
    mov r2, #0
    orr r2, r2, r3, lsl #27
    orr r2, r2, r3, lsl #22

#elif defined(ARM_CORTEX_A9)
    /* Before lockdown, victim = num_locked_tlb_entries. */
    mov r1, #1
    orr r1, r1, r3, lsl #28

    /* After lockdown, victim = num_locked_tlb_entries + 1. */
    add r3, r3, #1
    mov r2, #0
    orr r2, r2, r3, lsl #28
#else
#   error Undefined CPU for TLB lockdown
#endif

    str r3, tlb_lock_count

    /* Invalidate both I & D TLB entry */
    mcr p15, 0, r0, c8, c7, 1

    /* Now lock it! */
    mcr p15, 0, r1, c10, c0, 1 /* Start locking walked I-TLB entries. */
    mcr p15, 0, r0, c10, c1, 1 /* Load I-TLB entry. */
    mcr p15, 0, r2, c10, c0, 1 /* Finish locking walked I-TLB entries. */

    mcr p15, 0, r1, c10, c0, 0 /* Start locking walked D-TLB entries. */
    mcr p15, 0, r0, c10, c1, 0 /* Load D-TLB entry. */
    mcr p15, 0, r2, c10, c0, 0 /* Finish locking walked D-TLB entries. */

    /* 64-byte boundary is here (phew!) <------------------------ */

    bx lr

tlb_lock_count:
    .word 0
END_FUNC(lockTLBEntry)
#endif /* ARM_CORTEX_A15 */
