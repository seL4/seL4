/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#include <config.h>
#include <stdint.h>
#include <util.h>
#include <machine/io.h>

/* When DLAB=1, MU_IO is a baud rate register.
 * Otherwise, write to TX, read to RX */
#define MU_IO       0x00
/* When DLAB=1, MU_IIR is a baud rate register.
 * Otherwise IRQ enable */
#define MU_IIR      0x04
#define MU_IER      0x08
#define MU_LCR      0x0C
#define MU_MCR      0x10
#define MU_LSR      0x14
#define MU_MSR      0x18
#define MU_SCRATCH  0x1C
#define MU_CNTL     0x20

/* This bit is set if the transmit FIFO can accept at least one byte.*/
#define MU_LSR_TXEMPTY   BIT(5)
/* This bit is set if the transmit FIFO is empty and the
 * transmitter is idle. (Finished shifting out the last bit). */
#define MU_LSR_TXIDLE    BIT(6)
#define MU_LSR_RXOVERRUN BIT(1)
#define MU_LSR_DATAREADY BIT(0)
#define MU_LCR_DLAB      BIT(7)
#define MU_LCR_BREAK     BIT(6)
#define MU_LCR_DATASIZE  BIT(0)

#define UART_REG(x) ((volatile uint32_t *)(UART_PPTR + (x)))

#ifdef CONFIG_PRINTING
void init_console(void)
{
}

void uart_drv_putchar(unsigned char c)
{
    while (!(*UART_REG(MU_LSR) & MU_LSR_TXIDLE));
    *UART_REG(MU_IO) = (c & 0xff);
}
#endif /* CONFIG_PRINTING */

#ifdef CONFIG_DEBUG_BUILD
unsigned char uart_drv_getchar(void)
{
    while (!(*UART_REG(MU_LSR) & MU_LSR_DATAREADY));
    return *UART_REG(MU_IO);
}
#endif /* CONFIG_DEBUG_BUILD */
