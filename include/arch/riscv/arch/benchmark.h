/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 * Copyright 2015, 2016 Hesham Almatary <heshamelmatary@gmail.com>
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#pragma once

#include <config.h>
#include <arch/object/structures.h>

#ifdef CONFIG_ENABLE_BENCHMARK
#error "RISC-V doesn't support timestamp() function yet"
#endif /* CONFIG_ENABLE_BENCHMARK */

