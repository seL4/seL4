/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#include <config.h>
#include <mode/machine.h>
#include <api/debug.h>

/*
 * The idle thread currently does not receive a stack pointer and so we rely on
 * optimisations for correctness here. More specifically, we assume:
 *  - Ordinary prologue/epilogue stack operations are optimised out
 *  - All nested function calls are inlined
 * Note that GCC does not correctly implement optimisation annotations on nested
 * functions, so FORCE_INLINE is required on the wfi declaration in this case.
 * Note that Clang doesn't obey FORCE_O2 and relies on the kernel being compiled
 * with optimisations enabled.
 */
void FORCE_O2 idle_thread(void)
{
    while (1) {
        wfi();
    }
}

/** DONT_TRANSLATE */
void NORETURN NO_INLINE VISIBLE halt(void)
{
    /* halt is actually, idle thread without the interrupts */
    asm volatile("cpsid iaf");

#ifdef CONFIG_PRINTING
    printf("halting...");
#ifdef CONFIG_DEBUG_BUILD
    debug_printKernelEntryReason();
#endif
#endif
    idle_thread();
    UNREACHABLE();
}
