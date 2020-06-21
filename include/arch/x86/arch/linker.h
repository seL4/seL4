/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#pragma once

#include <util.h>

/* code that is linked to physical addresses */
#define PHYS_CODE SECTION(".phys.text")

/* data that is linked to physical addresses */
#define PHYS_DATA SECTION(".phys.data")

/* bss data that is linked to physical addresses */
#define PHYS_BSS SECTION(".phys.bss")

