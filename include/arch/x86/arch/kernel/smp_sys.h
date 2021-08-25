/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#pragma once
/* Lower memory address to copy APs boot code in real mode. Actual memory starts at
 * 0x500 but we need to round up to a page aligned address in order to send the
 * startup IPI */
#define BOOT_NODE_PADDR 0x1000
/* Limit of memory region we can copy the AP to */
#define BOOT_NODE_MAX_PADDR 0x7bff

#ifdef ENABLE_SMP_SUPPORT
void boot_node(void);
BOOT_CODE void start_boot_aps(void);
BOOT_CODE bool_t copy_boot_code_aps(uint32_t mem_lower);
#endif /* ENABLE_SMP_SUPPORT */

