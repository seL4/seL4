/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#ifndef __LIBSEL4_BOOTINFO_H
#define __LIBSEL4_BOOTINFO_H

#include <sel4/types.h>
#include <sel4/bootinfo_types.h>
#include <sel4/macros.h>

void seL4_InitBootInfo(seL4_BootInfo *bi)
SEL4_DEPRECATED("libsel4 management of bootinfo is deprecated, see the BootInfo Frame section of the manual");
seL4_BootInfo *seL4_GetBootInfo(void)
SEL4_DEPRECATED("libsel4 management of bootinfo is deprecated, see the BootInfo Frame section of the manual");

#endif // __LIBSEL4_BOOTINFO_H
