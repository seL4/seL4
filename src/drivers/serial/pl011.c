/*
 * Copyright 2016, General Dynamics C4 Systems
 * Copyright 2024, Capabilities Limited
 * CHERI support contributed by Capabilities Limited was developed by Hesham Almatary
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#include <config.h>
#include <stdint.h>
#include <util.h>
#include <machine/io.h>
#include <plat/machine/devices_gen.h>

#define UARTDR                    0x000
#define UARTFR                    0x018

#define PL011_UARTFR_TXFF         BIT(5)
#define PL011_UARTFR_RXFE         BIT(4)


#ifdef CONFIG_PRINTING
static volatile char *uart_map;
#define UART_REG(x) ((volatile uint32_t *) (uart_map + (x)))

void init_console(void)
{
#if defined(__CHERI_PURE_CAPABILITY__)
    uart_map = (volatile char *) cheri_build_device_cap((ptraddr_t) UART_PPTR, 28);
#else
    uart_map = (volatile char *) UART_PPTR;
#endif
}

void uart_drv_putchar(unsigned char c)
{
    while ((*UART_REG(UARTFR) & PL011_UARTFR_TXFF) != 0);

    *UART_REG(UARTDR) = c;
}
#endif /* CONFIG_PRINTING */

#ifdef CONFIG_DEBUG_BUILD
unsigned char uart_drv_getchar(void)
{
    while ((*UART_REG(UARTFR) & PL011_UARTFR_RXFE) != 0);

    return *UART_REG(UARTDR);
}
#endif  /* CONFIG_DEBUG_BUILD */
