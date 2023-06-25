/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 * Copyright (C) 2021, Hensoldt Cyber GmbH
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */
#pragma once

#include <sel4/config.h>
/* RasPi4 uses a BCM2711 SoC with 4x Cortex-A72. */
#include <sel4/arch/constants_cortex_a72.h>

#if CONFIG_WORD_SIZE == 32
/* First address in the virtual address space that is not accessible to user level */
#define seL4_UserTop 0xe0000000
#else
/* otherwise this is defined at the arch level */
#endif
