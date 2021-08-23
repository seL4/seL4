/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#include <config.h>
#include <arch/kernel/boot_sys.h>
#include <arch/model/statedata.h>
#include <machine/io.h>
#include <plat/machine/io.h>
#include <drivers/uart.h>

#if defined(CONFIG_DEBUG_BUILD) || defined(CONFIG_PRINTING)
void serial_init(uint16_t port)
{
    while (!(in8(port + 5) & 0x60)); /* wait until not busy */

    out8(port + 1, 0x00); /* disable generating interrupts */
    out8(port + 3, 0x80); /* line control register: command: set divisor */
    out8(port,     0x01); /* set low byte of divisor to 0x01 = 115200 baud */
    out8(port + 1, 0x00); /* set high byte of divisor to 0x00 */
    out8(port + 3, 0x03); /* line control register: set 8 bit, no parity, 1 stop bit */
    out8(port + 4, 0x0b); /* modem control register: set DTR/RTS/OUT2 */

    in8(port);     /* clear receiver port */
    in8(port + 5); /* clear line status port */
    in8(port + 6); /* clear modem status port */
}
#endif /* defined(CONFIG_DEBUG_BUILD) || defined(CONFIG_PRINTING) */

#ifdef CONFIG_PRINTING

/* there is no UART driver at drivers/serial/... for the pc99 platform, but we
 * implement parts of the API here, so we can reuse the generic code.
 */
void uart_drv_putchar(
    unsigned char c)
{
    while (x86KSdebugPort && (in8(x86KSdebugPort + 5) & 0x20) == 0);
    out8(x86KSdebugPort, c);
}

void kernel_putDebugChar(unsigned char c)
{
    /* this will take care of CR/LF handling and call uart_drv_putchar() */
    uart_console_putchar(c);
}

#endif /* CONFIG_PRINTING */

#ifdef CONFIG_DEBUG_BUILD
unsigned char kernel_getDebugChar(void)
{
    while ((in8(x86KSdebugPort + 5) & 1) == 0);
    return in8(x86KSdebugPort);
}
#endif /* CONFIG_DEBUG_BUILD */
