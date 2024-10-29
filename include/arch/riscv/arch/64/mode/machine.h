/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 * Copyright 2024, Capabilities Limited
 * CHERI support contributed by Capabilities Limited was developed by Hesham Almatary
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#pragma once

#include <util.h>
#include <arch/model/smp.h>
#include <stdint.h>
#include <plat/machine/devices_gen.h>

#ifdef CONFIG_RISCV_USE_CLINT_MTIME
/*
 * Currently all RISC-V 64-bit platforms supported have the mtime register
 * mapped at the same offset of the base address of the CLINT.
 */
#define CLINT_MTIME_OFFSET 0xbff8
#endif

static inline uint64_t riscv_read_time(void)
{
    word_t n;
#ifdef CONFIG_RISCV_USE_CLINT_MTIME
    n = *(volatile word_t *)(clint_pptr + CLINT_MTIME_OFFSET);
#else
    asm volatile("rdtime %0" : "=r"(n));
#endif
    return n;
}

static inline uint64_t riscv_read_cycle(void)
{
    word_t n;
    asm volatile("rdcycle %0" : "=r"(n));
    return n;
}
