/*
 * Copyright 2017, DornerWorks
 * Copyright 2014, General Dynamics C4 Systems
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(GD_DORNERWORKS_GPL)
 */

#include <config.h>
#include <stdint.h>
#include <util.h>
#include <machine/io.h>
#include <plat/machine/devices_gen.h>

#define XUARTPS_SR             0x2C
#define XUARTPS_FIFO           0x30

#define LPUART_VERID    0x00
#define LPUART_PARAM    0x04
#define LPUART_GLOBAL   0x08
#define LPUART_PINCFG   0x0C
#define LPUART_BAUD     0x10
#define LPUART_STAT     0x14
#define LPUART_CTRL     0x18
#define LPUART_DATA     0x1C
#define LPUART_MATCH    0x20
#define LPUART_FIFO     0x28
#define LPUART_WATER    0x2C

enum LPUART_VERID_BITS {
    VERID_FEATURE_START = 0,
    VERID_FEATURE_END   = 15,
    VERID_MINOR_START   = 16,
    VERID_MINOR_END     = 23,
    VERID_MAJOR_START   = 24,
    VERID_MAJOR_END     = 31
};

enum LPUART_PARAM_BITS {
    PARAM_RXFIFO_START = 8,
    PARAM_RXFIFO_END   = 15,
    PARAM_TXFIFO_START = 0,
    PARAM_TXFIFO_END   = 7,
};

enum LPUART_GLOBAL_BITS {
    GLOBAL_RST = 1
};

enum LPUART_PINCFG_BITS {
    PINCFG_TRGSEL_0 = 0,
    PINCFG_TRGSEL_1 = 1
};

enum LPUART_BAUD_BITS {
    BAUD_MAEN1        = 31,
    BAUD_MAEN2        = 30,
    BAUD_M10          = 29,
    BAUD_OSR_END      = 28,
    BAUD_OSR_START    = 24,
    BAUD_TDMAE        = 23,
    BAUD_RDMAE        = 21,
    BAUD_RIDMAE       = 20,
    BAUD_MATCFG_END   = 19,
    BAUD_MATCFG_START = 18,
    BAUD_BOTHEDGE     = 17,
    BAUD_RESYNCDIS    = 16,
    BAUD_LBKDIE       = 15,
    BAUD_RXEDGIE      = 14,
    BAUD_SBNS         = 13,
    BAUD_SBR_END      = 12,
    BAUD_SBR_START    = 0
};

enum LPUART_STAT_BITS {
    STAT_LBKDIF  = 31,
    STAT_RXEDGIF = 30,
    STAT_MSBF    = 29,
    STAT_RXINV   = 28,
    STAT_RWUID   = 27,
    STAT_BRK13   = 26,
    STAT_LBKDE   = 25,
    STAT_RAF     = 24,
    STAT_TDRE    = 23,
    STAT_TC      = 22,
    STAT_RDRF    = 21,
    STAT_IDLE    = 20,
    STAT_OR      = 19,
    STAT_NF      = 18,
    STAT_FE      = 17,
    STAT_PF      = 16,
    STAT_MA1F    = 15,
    STAT_MA2F    = 14
};

enum LPUART_CTRL_BITS {
    CTRL_R8T9          = 31,
    CTRL_R9T8          = 30,
    CTRL_TXDIR         = 29,
    CTRL_TXINV         = 28,
    CTRL_ORIE          = 27,
    CTRL_NEIE          = 26,
    CTRL_FEIE          = 25,
    CTRL_PEIE          = 24,
    CTRL_TIE           = 23,
    CTRL_TCIE          = 22,
    CTRL_RIE           = 21,
    CTRL_ILIE          = 20,
    CTRL_TE            = 19,
    CTRL_RE            = 18,
    CTRL_RWU           = 17,
    CTRL_SBK           = 16,
    CTRL_MA1IE         = 15,
    CTRL_MA2IE         = 14,
    CTRL_M7            = 11,
    CTRL_IDLECFG_END   = 10,
    CTRL_IDLECFG_START = 8,
    CTRL_LOOPS         = 7,
    CTRL_DOZEEN        = 6,
    CTRL_RSRC          = 5,
    CTRL_M             = 4,
    CTRL_WAKE          = 3,
    CTRL_ILT           = 2,
    CTRL_PE            = 1,
    CTRL_PT            = 0
};

enum LPUART_DATA_BITS {
    DATA_NOISY   = 15,
    DATA_PARITYE = 14,
    DATA_FRETSC  = 13,
    DATA_RXEMPT  = 12,
    DATA_IDLINE  = 11,
    DATA_R9T9    = 9,
    DATA_R8T8    = 8,
    DATA_R7T7    = 7,
    DATA_R6T6    = 6,
    DATA_R5T5    = 5,
    DATA_R4T4    = 4,
    DATA_R3T3    = 3,
    DATA_R2T2    = 2,
    DATA_R1T1    = 1,
    DATA_R0T0    = 0
};

enum LPUART_MATCH_BITS {
    MATCH_MA2_END   = 31,
    MATCH_MA2_START = 26,
    MATCH_MA1_END   = 9,
    MATCH_MA1_START = 0,
};

enum LPUART_FIFO_BITS {
    FIFO_TXEMPT           = 23,
    FIFO_RXEMPT           = 22,
    FIFO_TXOF             = 17,
    FIFO_RXUF             = 16,
    FIFO_TXFLUSH          = 15,
    FIFO_RXFLUSH          = 14,
    FIFO_RXIDEN_END       = 12,
    FIFO_RXIDEN_START     = 10,
    FIFO_TXOFE            = 9,
    FIFO_RXUFE            = 8,
    FIFO_TXFE             = 7,
    FIFO_TXFIFOSIZE_END   = 6,
    FIFO_TXFIFOSIZE_START = 4,
    FIFO_RXFE             = 3,
    FIFO_RXFIFOSIZE_END   = 2,
    FIFO_RXFIFOSIZE_START = 0,
};

enum LPUART_WATERMARK_BITS {
    WATERMARK_RXCOUNT_END   = 30,
    WATERMARK_RXCOUND_START = 24,
    WATERMARK_RXWATER_END   = 21,
    WATERMARK_RXWATER_START = 16,
    WATERMARK_TXCOUNT_END   = 14,
    WATERMARK_TXCOUNT_START = 8,
    WATERMARK_TXWATER_END   = 5,
    WATERMARK_TXWATER_START = 0
};


#define XUARTPS_SR_TXEMPTY     (1U << 3)

#define UART_REG(x) ((volatile uint32_t *)(UART_PPTR + (x)))

#if defined(CONFIG_DEBUG_BUILD) || defined(CONFIG_PRINTING)
void putDebugChar(unsigned char c)
{
    while (!(*UART_REG(LPUART_STAT) & BIT(STAT_TDRE)));
    *UART_REG(LPUART_DATA) = c;
}
#endif


#ifdef CONFIG_DEBUG_BUILD
unsigned char getDebugChar(void)
{
    while (!(*UART_REG(LPUART_STAT) & BIT(STAT_RDRF)));
    return *UART_REG(LPUART_DATA);
}
#endif /* CONFIG_DEBUG_BUILD */
