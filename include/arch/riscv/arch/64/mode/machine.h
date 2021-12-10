/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#pragma once

#include <util.h>
#include <arch/model/smp.h>
#include <stdint.h>

static inline uint64_t riscv_read_time(void)
{
    word_t n;
    asm volatile("rdtime %0" : "=r"(n));
    return n;
}

static inline uint64_t riscv_read_cycle(void)
{
    word_t n;
    asm volatile("rdcycle %0" : "=r"(n));
    return n;
}
