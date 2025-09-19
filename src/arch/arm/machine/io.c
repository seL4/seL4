/*
 * Copyright 2021, Axel Heider <axelheider@gmx.de>
 *
 * SPDX-License-Identifier: GPL-2.0-or-later
 */

#include <config.h>
#include <machine/io.h>
#include <drivers/uart.h>

#ifdef CONFIG_PRINTING

/**
 * This is initially the PADDR for the early kernel boots, and we switch it
 * out to use the PPTR once the kernel has mapped memory in.
 */
pptr_t uart_pptr = UART_PADDR;

void kernel_putDebugChar(unsigned char c)
{
    uart_console_putchar(c);
}
#endif /* CONFIG_PRINTING */

#ifdef CONFIG_DEBUG_BUILD
unsigned char kernel_getDebugChar(void)
{
    return uart_drv_getchar();
}
#endif /* CONFIG_DEBUG_BUILD */
