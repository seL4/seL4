/*
 * Copyright 2022, tyyteam(Qingtao Liu, Yang Lei, Yang Chen)
 * qtliu@mail.ustc.edu.cn, le24@mail.ustc.edu.cn, chenyangcs@mail.ustc.edu.cn
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#include <config.h>
#include <stdint.h>
#include <util.h>
#include <machine/io.h>

#define UART_REG_DAT 0x00
#define UART_REG_IER 0x01
#define UART_REG_IIR 0x02
#define UART_REG_FCR 0x02
#define UART_REG_LCR 0x03
#define UART_REG_MCR 0x04
#define UART_REG_LSR 0x05
#define UART_REG_MSR 0x06

#define UART_REG_LSR_TFE BIT(5)
#define UART_REG_LSR_DR BIT(0)

#define UART_REG(x) ((volatile uint8_t *)((UART_PPTR) + (x)))

#ifdef CONFIG_PRINTING
void uart_drv_putchar(unsigned char c)
{
    while(!(*UART_REG(UART_REG_LSR) & UART_REG_LSR_TFE));
    *UART_REG(UART_REG_DAT) = (c & 0xff);
}
#endif /* CONFIG_PRINTING */

#ifdef CONFIG_DEBUG_BUILD
unsigned char uart_drv_getchar(void)
{
    while(!(*UART_REG(UART_REG_LSR) & UART_REG_LSR_DR));
    return *UART_REG(UART_REG_DAT);
}
#endif /* CONFIG_DEBUG_BUILD */





