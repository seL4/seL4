/*
 * Copyright 2022, tyyteam
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

/*
 * LoongArch maintains ICache/DCache coherency by hardware,
 * we just need "ibar" to avoid instruction hazard here.
 */
#include <arch/machine/cache.h>

void local_flush_icache_range(unsigned long start, unsigned long end)
{
	asm volatile ("\tibar 0\n"::);
}

// void cache_error_setup(void)
// {
// 	extern char __weak except_vec_cex;
// 	set_merr_handler(0x0, &except_vec_cex, 0x80);
// }