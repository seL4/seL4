/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 * Copyright 2015, 2016 Hesham Almatary <heshamelmatary@gmail.com>
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#include <machine/io.h>
#include <arch/sbi.h>

#if defined(CONFIG_PRINTING) || defined(CONFIG_DEBUG_BUILD)
void putDebugChar(unsigned char c)
{
    sbi_console_putchar(c);
}
#endif
