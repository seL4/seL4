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
#define UART_REG(X) ((volatile uint32_t *)(UART_PPTR + (X)))

#define USR                   0x08
#define UTF                   0x70
#define UNTX                  0x40

#define USR_TXRDY             (1U << 2)
#define USR_TXEMP             (1U << 3)


void
apq8064_uart_putchar(char c)
{
    putDebugChar(c);
    if (c == '\n') {
        putDebugChar('\r');
    }
}

void
putDebugChar(unsigned char c)
{
    while ( (*UART_REG(USR) & USR_TXEMP) == 0 );
    /* Tell the peripheral how many characters to send */
    *UART_REG(UNTX) = 1;
    /* Write the character into the FIFO */
    *UART_REG(UTF) = c & 0xff;
}

unsigned char
getDebugChar(void)
{
    return -1;
}


#endif /* DEBUG */
