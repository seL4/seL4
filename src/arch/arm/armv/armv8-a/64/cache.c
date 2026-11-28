/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#include <config.h>
#include <arch/machine.h>
#include <arch/machine/hardware.h>
#include <kernel/boot.h>

static void get_boot_flush_region(word_t *start, word_t *end)
{
    *start = (word_t)ptrFromPAddr(physBase());

    if (rootserver.paging.end != 0) {
        /* After init_freemem: flush up to rootserver allocations + margin */
        *end = rootserver.paging.end + (2 * 1024 * 1024);
    } else {
        /* Early boot: rootserver not yet initialized, flush kernel image */
        *end = (word_t)ki_end + (2 * 1024 * 1024);
    }
}

void clean_D_PoU(void)
{
    word_t start, end;
    get_boot_flush_region(&start, &end);
    cleanInvalidateCacheRange_RAM(start, end - 1, addrFromKPPtr((void *)start));
}

void cleanInvalidate_D_PoC(void)
{
    word_t start, end;
    get_boot_flush_region(&start, &end);
    cleanInvalidateCacheRange_RAM(start, end - 1, addrFromKPPtr((void *)start));
}

void cleanInvalidate_L1D(void)
{
    word_t start, end;
    get_boot_flush_region(&start, &end);
    cleanInvalidateCacheRange_RAM(start, end - 1, addrFromKPPtr((void *)start));
}
