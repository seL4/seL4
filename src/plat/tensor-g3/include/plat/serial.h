*
*
* Copyright 2025, Millpex
*
#pragma once

/*
 * UART Base Address for Tensor G3.
 * Source: dmesg.txt shows an 'exynos4210' compatible UART at this address.
 */
#define G3_UART_BASE 0x10870000

/*
 * Register offsets for the Exynos4-compatible UART.
 * Naming convention is harmonized to reflect the actual hardware.
 */
#define EXYNOS_UART_ULCON      0x00  // Line Control
#define EXYNOS_UART_UCON       0x04  // Control
#define EXYNOS_UART_UFCON      0x08  // FIFO Control
#define EXYNOS_UART_UTRSTAT    0x10  // Tx/Rx Status
#define EXYNOS_UART_UFSTAT     0x18  // FIFO Status
#define EXYNOS_UART_UTXH       0x20  // Transmit Holding Register
#define EXYNOS_UART_URXH       0x24  // Receive Buffer Register
#define EXYNOS_UART_UBRDIV     0x28  // Baud Rate Divisor
#define EXYNOS_UART_UFRACVAL   0x2C  // Fractional Baud Rate Divisor

/* UTRSTAT Bits (Tx/Rx Status) */
#define UART_TRSTAT_RX_READY   (1 << 0) // Receive buffer data ready
#define UART_TRSTAT_TX_EMPTY   (1 << 1) // Transmitter is completely empty (shift register)
#define UART_TRSTAT_TXB_EMPTY  (1 << 2) // Transmit buffer is empty

/* ULCON Bits (Line Control for 8N1) */
#define UART_LCON_8N1          0x3       // 8 data bits, no parity, 1 stop bit

/* UCON Bits (Control) */
#define UART_UCON_RX_IRQ_LEVEL (1 << 0) // RX Interrupt request type: Level
#define UART_UCON_TX_IRQ_LEVEL (1 << 2) // TX Interrupt request type: Level

/* UFCON Bits (FIFO Control) */
#define UART_FCON_FIFO_ENABLE  (1 << 0) // Enable FIFOs
#define UART_FCON_RX_FIFO_RESET (1 << 1) // Reset RX FIFO
#define UART_FCON_TX_FIFO_RESET (1 << 2) // Reset TX FIFO
