/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#ifndef __ARCH_LINKER_H
#define __ARCH_LINKER_H

#include <util.h>

/* code that is linked to physical addresses */
#define PHYS_CODE SECTION(".phys.text")

/* data that is linked to physical addresses */
#define PHYS_DATA SECTION(".phys.data")

/* bss data that is linked to physical addresses */
#define PHYS_BSS SECTION(".phys.bss")

#endif /* __ARCH_LINKER_H */
