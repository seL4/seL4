/*
 * Copyright 2016, General Dynamics C4 Systems
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

#if defined DEBUG || defined RELEASE_PRINTF

#define UTHR        0x0
#define ULSR        0x14
#define ULSR_THRE   (1 << 5)

#define UART_REG(x) ((volatile uint32_t *)(UARTD_PPTR + (x)))

void
tk1_uart_putchar(char c)
{
    while ((*UART_REG(ULSR) & ULSR_THRE) == 0);

    *UART_REG(UTHR) = (c & 0xff);

    if (c == '\n') {
        tk1_uart_putchar('\r');
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
