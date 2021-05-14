/*
 * Copyright 2021, Axel Heider <axelheider@gmx.de>
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 *
 * SPDX-License-Identifier: GPL-2.0-or-later
 */

#include <config.h>

#ifdef CONFIG_DEBUG_BUILD

#include <machine/capdl.h>
#include <arch/machine/capdl.h>

void debug_capDL(void)
{
#ifdef CONFIG_PRINTING
    printf("Debug CapDL snapshot not full implemented for IA32\n");
#endif /* CONFIG_PRINTING */
    /* reset the seen list */
    reset_seen_list();
}

#endif /* CONFIG_DEBUG_BUILD */
