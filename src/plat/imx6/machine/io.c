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

#define URXD  0x00 /* UART Receiver Register */
#define UTXD  0x40 /* UART Transmitter Register */
#define UCR1  0x80 /* UART Control Register 1 */
#define UCR2  0x84 /* UART Control Register 2 */
#define UCR3  0x88 /* UART Control Register 3 */
#define UCR4  0x8c /* UART Control Register 4 */
#define UFCR  0x90 /* UART FIFO Control Register */
#define USR1  0x94 /* UART Status Register 1 */
#define USR2  0x98 /* UART Status Register 2 */
#define UESC  0x9c /* UART Escape Character Register */
#define UTIM  0xa0 /* UART Escape Timer Register */
#define UBIR  0xa4 /* UART BRM Incremental Register */
#define UBMR  0xa8 /* UART BRM Modulator Register */
#define UBRC  0xac /* UART Baud Rate Counter Register */
#define ONEMS 0xb0 /* UART One Millisecond Register */
#define UTS   0xb4 /* UART Test Register */

#define UART_REG(x) ((volatile uint32_t *)(UART_PPTR + (x)))


#define UART_SR2_TXFIFO_EMPTY 14
#define UART_SR2_RXFIFO_RDR    0

void
imx6_uart_putchar(char c)
{
    putDebugChar(c);
    if (c == '\n') {
        putDebugChar('\r');
    }
}

void putDebugChar(unsigned char c)
{
    while (!(*UART_REG(USR2) & BIT(UART_SR2_TXFIFO_EMPTY)));
    *UART_REG(UTXD) = c;
}

unsigned char getDebugChar(void)
{
    while (!(*UART_REG(USR2) & BIT(UART_SR2_RXFIFO_RDR)));
    return *UART_REG(URXD);
}

#endif /* DEBUG */
