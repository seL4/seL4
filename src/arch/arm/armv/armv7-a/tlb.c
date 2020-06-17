/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#include <arch/machine/hardware.h>

#if defined(CONFIG_ARM_CORTEX_A15) || defined(CONFIG_ARM_CORTEX_A7)
/* The hardware does not support tlb locking */
void lockTLBEntry(vptr_t vaddr)
{

}

#else

void lockTLBEntry(vptr_t vaddr)
{
    int n = tlbLockCount;
    int x, y;

    tlbLockCount ++;
    /* Compute two values, x and y, to write to the lockdown register. */

#if defined(CONFIG_ARM_CORTEX_A8)

    /* Before lockdown, base = victim = num_locked_tlb_entries. */
    x = 1 | (n << 22) | (n << 27);
    n ++;
    /* After lockdown, base = victim = num_locked_tlb_entries + 1. */
    y = (n << 22) | (n << 27);

#elif defined(CONFIG_ARM_CORTEX_A9)

    /* Before lockdown, victim = num_locked_tlb_entries. */
    x = 1 | (n << 28);
    n ++;
    /* After lockdown, victim = num_locked_tlb_entries + 1. */
    y = (n << 28);

#else

    userError("Undefined CPU for TLB lockdown.\n");
    halt();

#endif /* A8/A9 */

    lockTLBEntryCritical(vaddr, x, y);
}

#endif /* A15/A7 */


