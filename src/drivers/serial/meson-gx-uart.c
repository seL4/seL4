/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#include <config.h>
#include <stdint.h>
#include <string.h>
#include <util.h>
#include <machine/io.h>
#include <plat/machine/devices_gen.h>

#define UART_WFIFO  0x0
#define UART_RFIFO  0x4
#define UART_CTRL   0x8
#define UART_STATUS 0xC
#define UART_MISC   0x10

#define UART_TX_FULL        BIT(21)
#define UART_RX_EMPTY       BIT(20)

#define UART_RX_IRQ         BIT(27)
#define UART_RX_EN          BIT(13)
#define UART_TX_EN          BIT(12)

#define UART_REG(x) ((volatile uint32_t *)(UART_PPTR + (x)))

#define WDOG_EN BIT(18)
#define WDOG_SYS_RESET_EN BIT(21)
#define WDOG_CLK_EN BIT(24)
#define WDOG_CLK_DIV_EN BIT(25)
#define WDOG_SYS_RESET_NOW BIT(26)

static const char *reset = "reset";
static int index = 0;

void uart_drv_putchar(unsigned char c);

struct UARTRecvBuf {
    unsigned int head;
    unsigned int tail;
    char data[2048];
};

struct UARTRecvBuf *uart_recv_buf;
cap_t signal_ntfn = { 0 };

void init_serial(cap_t uart_recv_cap)
{
    /* enable tx, rx and rx irq */
    *(UART_REG(UART_CTRL)) |= UART_TX_EN | UART_RX_EN | UART_RX_IRQ;
    /* send irq when 1 char is available */
    *(UART_REG(UART_MISC)) = 1;
    uart_recv_buf = (struct UARTRecvBuf *) cap_frame_cap_get_capFBasePtr(uart_recv_cap);
}

void serial_set_ntfn(cap_t ntfn) {
    signal_ntfn = ntfn;
}

void wdog_reset(void)
{
    printf("\nResetting Odroid-C2\n");
    volatile uint32_t *wdog = (volatile uint32_t *)(WDOG_PPTR);
    *wdog = (WDOG_EN | WDOG_SYS_RESET_EN | WDOG_CLK_EN |
             WDOG_CLK_DIV_EN | WDOG_SYS_RESET_NOW);
}


void handleUartIRQ(void)
{
    /* while there are chars to process */
    while (!(*UART_REG(UART_STATUS) & UART_RX_EMPTY)) {
        char c = *UART_REG(UART_RFIFO);

        /* Write to the userspace buffer */
        uart_recv_buf->data[uart_recv_buf->head % 2048] = c;
        uart_recv_buf->head++;

        if (cap_get_capType(signal_ntfn) == cap_notification_cap) {
            sendSignal(NTFN_PTR(cap_notification_cap_get_capNtfnPtr(signal_ntfn)),
                       cap_notification_cap_get_capNtfnBadge(signal_ntfn));
        }

        // uart_drv_putchar(c);
        if (c == 'r') {
            index = 1;
        } else if (c == reset[index]) {
            index++;
        } else {
            index = 0;
        }
        if (index == strnlen(reset, 5)) {
            /* do the reset */
            wdog_reset();
        }
    }
}

void uart_drv_putchar(unsigned char c)
{
    while ((*UART_REG(UART_STATUS) & UART_TX_FULL));

    /* Add character to the buffer. */
    *UART_REG(UART_WFIFO) = c;
}

#ifdef CONFIG_DEBUG_BUILD
unsigned char uart_drv_getchar(void)
{
    while ((*UART_REG(UART_STATUS) & UART_RX_EMPTY));
    return *UART_REG(UART_RFIFO);
}
#endif /* CONFIG_DEBUG_BUILD */
