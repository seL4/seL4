/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#include <arch/machine/hardware.h>

#if defined(CONFIG_ARM_CORTEX_A8)

void lockTLBEntry(vptr_t vaddr)
{
    int n = tlbLockCount;
    int x, y;

    tlbLockCount ++;
    /* Compute two values, x and y, to write to the lockdown register. */

    /* Before lockdown, base = victim = num_locked_tlb_entries. */
    x = 1 | (n << 22) | (n << 27);
    n ++;
    /* After lockdown, base = victim = num_locked_tlb_entries + 1. */
    y = (n << 22) | (n << 27);

    lockTLBEntryCritical(vaddr, x, y);
}

/* if CORTEX_A8 */
#else

/* We don't currently support TLB locking for other processors. */
void lockTLBEntry(vptr_t vaddr)
{

}

#endif
