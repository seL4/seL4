/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(GD_GPL)
 */

#include <stdint.h>
#include <util.h>
#include <machine/io.h>
#include <plat/machine/devices.h>

#ifdef DEBUG

#define UTHR 0x00 /* UART Transmit Holding Register */
#define ULSR 0x14 /* UART Line Status Register */
#define ULSR_THRE 0x20 /* Transmit Holding Register Empty */

#define UART_REG(x) ((volatile uint32_t *)(UART3_PPTR + (x)))


void
omap3_uart_putchar(char c)
{
    while ((*UART_REG(ULSR) & ULSR_THRE) == 0);

    *UART_REG(UTHR) = c;

    if (c == '\n') {
        omap3_uart_putchar('\r');
    }
}

void putDebugChar(unsigned char c)
{
    while ((*UART_REG(ULSR) & ULSR_THRE) == 0);

    *UART_REG(UTHR) = c;
}

unsigned char getDebugChar(void)
{
    return 0;
}

#endif
