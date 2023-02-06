/*
 * Copyright 2022, tyyteam
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */
#include <arch/machine.h>
#include <arch/machine/tlb.h>
#include <linker.h>
#include <string.h>
#include <arch/kernel/traps.h>
#include <arch/machine/cache.h>

void local_flush_tlb_all(void)
{
	invtlb_all(INVTLB_CURRENT_ALL, 0, 0);
}

extern void handle_tlb_refill(void);

void setup_tlb_handler(void)
{
    write_csr_tlbrentry(kpptr_to_paddr(handle_tlb_refill));
}

BOOT_CODE void init_tlb(void)
{
    write_csr_pagesize(PS_DEFAULT_SIZE);
    write_csr_stlbpgsize(PS_DEFAULT_SIZE);
    write_csr_tlbrefill_pagesize(PS_HUGE_SIZE);

    if (read_csr_pagesize() != PS_DEFAULT_SIZE)
        printf("MMU doesn't support PAGE_SIZE\n");

    setup_tlb_handler();

    // local_flush_tlb_all();
}
