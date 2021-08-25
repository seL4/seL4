/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#pragma once

#include <plat/machine/devices_gen.h>
#include <machine/io.h>
#include <machine/interrupt.h>

#define BASIC_IRQ_OFFSET                32
#define NORMAL_IRQ_OFFSET               (BASIC_IRQ_OFFSET + 32)

enum {
    INTERRUPT_CORE_CNTPSIRQ                  =  0,
    INTERRUPT_CORE_CNTPNSIRQ                 =  1,
    INTERRUPT_CORE_CNTHPIRQ                  =  2,
    INTERRUPT_CORE_CNTVIRQ                   =  3,
    INTERRUPT_CORE_MAILBOX_0                 =  4,
    INTERRUPT_CORE_MAILBOX_1                 =  5,
    INTERRUPT_CORE_MAILBOX_2                 =  6,
    INTERRUPT_CORE_MAILBOX_3                 =  7,
    INTERRUPT_CORE_GPU                       =  8,
    INTERRUPT_CORE_PMU                       =  9,
    INTERRUPT_CORE_AXI                       = 10,
    INTERRUPT_CORE_LOCAL_TIMER               = 11,
    //17:12 Peripheral 1..15 interrupt (Currently not used)
    //31:28 <Reserved>

    INTERRUPT_BASIC_IRQ_ARM_TIMER            = (BASIC_IRQ_OFFSET + 0),
    INTERRUPT_BASIC_IRQ_ARM_MAILBOX          = (BASIC_IRQ_OFFSET + 1),
    INTERRUPT_BASIC_IRQ_ARM_DOORBELL0        = (BASIC_IRQ_OFFSET + 2),
    INTERRUPT_BASIC_IRQ_ARM_DOORBELL1        = (BASIC_IRQ_OFFSET + 3),
    INTERRUPT_BASIC_IRQ_GPU0_HALTED          = (BASIC_IRQ_OFFSET + 4),
    INTERRUPT_BASIC_IRQ_GPU1_HALTED          = (BASIC_IRQ_OFFSET + 5),
    INTERRUPT_BASIC_IRQ_ILLEGAL_ACCESS_TYPE1 = (BASIC_IRQ_OFFSET + 6),
    INTERRUPT_BASIC_IRQ_ILLEGAL_ACCESS_TYPE0 = (BASIC_IRQ_OFFSET + 7),
    INTERRUPT_BASIC_IRQ_PENDING_REGISTER1    = (BASIC_IRQ_OFFSET + 8),
    INTERRUPT_BASIC_IRQ_PENDING_REGISTER2    = (BASIC_IRQ_OFFSET + 9),
    INTERRUPT_BASIC_IRQ_GPU_IRQ_7            = (BASIC_IRQ_OFFSET + 10),
    INTERRUPT_BASIC_IRQ_GPU_IRQ_9            = (BASIC_IRQ_OFFSET + 11),
    INTERRUPT_BASIC_IRQ_GPU_IRQ_10           = (BASIC_IRQ_OFFSET + 12),
    INTERRUPT_BASIC_IRQ_GPU_IRQ_18           = (BASIC_IRQ_OFFSET + 13),
    INTERRUPT_BASIC_IRQ_GPU_IRQ_19           = (BASIC_IRQ_OFFSET + 14),
    INTERRUPT_BASIC_IRQ_GPU_IRQ_53           = (BASIC_IRQ_OFFSET + 15),
    INTERRUPT_BASIC_IRQ_GPU_IRQ_54           = (BASIC_IRQ_OFFSET + 16),
    INTERRUPT_BASIC_IRQ_GPU_IRQ_55           = (BASIC_IRQ_OFFSET + 17),
    INTERRUPT_BASIC_IRQ_GPU_IRQ_56           = (BASIC_IRQ_OFFSET + 18),
    INTERRUPT_BASIC_IRQ_GPU_IRQ_57           = (BASIC_IRQ_OFFSET + 19),
    INTERRUPT_BASIC_IRQ_GPU_IRQ_62           = (BASIC_IRQ_OFFSET + 20),
    // 31:21 <unused>

    INTERRUPT_IRQ_AUX                        = (NORMAL_IRQ_OFFSET + 29),
    INTERRUPT_IRQ_I2C_SPI_SLV                = (NORMAL_IRQ_OFFSET + 43),
    INTERRUPT_IRQ_PWA0                       = (NORMAL_IRQ_OFFSET + 45),
    INTERRUPT_IRQ_PWA1                       = (NORMAL_IRQ_OFFSET + 46),
    INTERRUPT_IRQ_SMI                        = (NORMAL_IRQ_OFFSET + 48),
    INTERRUPT_IRQ_GPIO0                      = (NORMAL_IRQ_OFFSET + 49),
    INTERRUPT_IRQ_GPIO1                      = (NORMAL_IRQ_OFFSET + 50),
    INTERRUPT_IRQ_GPIO2                      = (NORMAL_IRQ_OFFSET + 51),
    INTERRUPT_IRQ_GPIO3                      = (NORMAL_IRQ_OFFSET + 52),
    INTERRUPT_IRQ_I2C                        = (NORMAL_IRQ_OFFSET + 53),
    INTERRUPT_IRQ_SPI                        = (NORMAL_IRQ_OFFSET + 54),
    INTERRUPT_IRQ_PCM                        = (NORMAL_IRQ_OFFSET + 55),
    INTERRUPT_IRQ_UART                       = (NORMAL_IRQ_OFFSET + 57),
};

#define FIQCTRL_FIQ_ENABLE                   BIT(7)
#define FIQCTRL_FIQ_SRC_GPU_IRQ(x)           (x)
#define FIQCTRL_FIQ_SRC_ARM_TIMER            64
#define FIQCTRL_FIQ_SRC_ARM_MAILBOX          65
#define FIQCTRL_FIQ_SRC_ARM_DOORBELL0        66
#define FIQCTRL_FIQ_SRC_ARM_DOORBELL1        67
#define FIQCTRL_FIQ_SRC_GPU0_HALTED          68
#define FIQCTRL_FIQ_SRC_GPU1_HALTED          69
#define FIQCTRL_FIQ_SRC_ILLEGAL_ACCESS_TYPE1 70
#define FIQCTRL_FIQ_SRC_ILLEGAL_ACCESS_TYPE0 71
#define FIQCTRL_FIQ_SRC(src)                 (FIQCTRL_FIQ_SRC_##src)

volatile struct intc_regs {
    uint32_t bfIRQBasicPending;  /* 0x200 R     */
    uint32_t bfGPUIRQPending[2]; /* 0x204 R     */
    uint32_t FIQ_control;        /* 0x20C R/W   */
    uint32_t bfEnableIRQs[2];    /* 0x210 R/Wbs */
    uint32_t bfEnableBasicIRQs;  /* 0x218 R/Wbs */
    uint32_t bfDisableIRQs[2];   /* 0x21C R/Wbc */
    uint32_t bfDisableBasicIRQs; /* 0x224 R/Wbc */
} *intc_regs = (volatile struct intc_regs *)INTC_PPTR;

volatile struct core_regs {
    uint32_t controlRegister;           /* 0x00 */
    uint32_t unused0;                  /* 0x04 */
    uint32_t coreTimerPrescaler;        /* 0x08 */
    uint32_t gpuInterruptsRouting;      /* 0x0C */
    uint32_t pmirSet;                   /* 0x10 */
    uint32_t pmirClear;                 /* 0x14 */
    uint32_t unused1;                  /* 0x18 */
    uint32_t coreTimerAccessLS;         /* 0x1C */
    uint32_t coreTimerAccessMS;         /* 0x20 */
    uint32_t localInterrupt0Routing;    /* 0x24 */
    uint32_t unused2;                  /* 0x28 */
    uint32_t axiOutstandingCounters;    /* 0x2C */
    uint32_t axiOutstandingIRQ;         /* 0x30 */
    uint32_t localTimerCtl;             /* 0x34 */
    uint32_t localTimerFlags;           /* 0x38 */
    uint32_t unused3;                  /* 0x3C */
    uint32_t coreTimersIrqCtrl[4];      /* 0x40 Timers interrupt control registers */
    uint32_t coreMailboxesIrqCtrl[4];   /* 0x50 Mailbox interrupt control */
    uint32_t coreIRQSource[4];          /* 0x60 IRQ source registers */
    uint32_t coreFIQSource[4];          /* 0x70 FIQ source registers */
    uint32_t coreMailboxWriteset[4][4]; /* 0x80 Mailbox write-set registers (Write only) */
    uint32_t coreMailboxRW[4][4];       /* 0xC0 Mailbox write-clear registers (Read & Write) */
} *core_regs = (volatile struct core_regs *) ARM_LOCAL_PPTR;

#define LOCAL_TIMER_IRQ_STATUS  31
#define LOCAL_TIMER_CTRL_IRQ_BIT 29
#define LOCAL_TIMER_CTRL_EN_BIT 28
#define LOCAL_TIMER_CTRL_RL_MASK MASK(28)

enum irqNumbers {
    irqInvalid = 255
};

static inline bool_t isIRQPending(void)
{
    uint32_t pending;
    pending = core_regs->coreIRQSource[0];

    /* Mask out invalid bits */
    pending &= MASK(12);
    return pending != 0;
}

static inline void ackInterrupt(UNUSED irq_t irq)
{
    /* No way to ACK an interrupt */
}

static inline void handleSpuriousIRQ(void)
{
    /* Nothing to do here */
}


