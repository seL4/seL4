/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 * Copyright 2015, 2016 Hesham Almatary <heshamelmatary@gmail.com>
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#pragma once

#include <config.h>
#include <arch/object/structures.h>
#include <mode/hardware.h>

#ifdef CONFIG_ENABLE_BENCHMARKS
static inline timestamp_t timestamp(void)
{
    return riscv_read_cycle();
}
#endif /* CONFIG_ENABLE_BENCHMARK */

