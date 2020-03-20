/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 * Copyright 2015, 2016 Hesham Almatary <heshamelmatary@gmail.com>
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#pragma once

#ifdef HAVE_AUTOCONF
#include <autoconf.h>
#endif /* HAVE_AUTOCONF */

typedef enum _mode_object {
    seL4_RISCV_Giga_Page = seL4_NonArchObjectTypeCount,
#if CONFIG_PT_LEVELS > 3
    seL4_RISCV_Tera_Page,
#endif
    seL4_ModeObjectTypeCount
} seL4_ModeObjectType;

#if CONFIG_PT_LEVELS <= 3
#define seL4_RISCV_Tera_Page 0xffffffff
#endif

