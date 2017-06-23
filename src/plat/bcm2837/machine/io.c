/*
 * Copyright 2017, Data61
 * Commonwealth Scientific and Industrial Research Organisation (CSIRO)
 * ABN 41 687 119 230.
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(DATA61_GPL)
 */

#include <config.h>
#include <stdint.h>
#include <util.h>
#include <machine/io.h>
#include <plat/machine/devices.h>

#define UART_REG(x) ((volatile uint32_t *)(UART_PPTR + (x)))

/* When DLAB=1, MU_IO is a baud rate register.
 * Otherwise, write to TX, read to RX */
#define MU_IO       0x40
/* When DLAB=1, MU_IIR is a baud rate register.
 * Otherwise IRQ enable */
#define MU_IIR      0x44
#define MU_IER      0x48
#define MU_LCR      0x4C
#define MU_MCR      0x50
#define MU_LSR      0x54
#define MU_MSR      0x58
#define MU_SCRATCH  0x5C
#define MU_CNTL     0x60


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


#if defined(CONFIG_DEBUG_BUILD) || defined(CONFIG_PRINTING)
void putDebugChar(unsigned char c)
{
    while ( !(*UART_REG(MU_LSR) & MU_LSR_TXIDLE) );
    *UART_REG(MU_IO) = (c & 0xff);
}
#endif

#ifdef CONFIG_DEBUG_BUILD
unsigned char getDebugChar(void)
{
    while ( !(*UART_REG(MU_LSR) & MU_LSR_DATAREADY) );
    return *UART_REG(MU_IO);
}
#endif /* CONFIG_DEBUG_BUILD */
