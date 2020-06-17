/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#pragma once

#define TMR0_IRQ_PEND_FLAG          BIT(0)

struct timer {
    uint32_t tmr_irq_en_reg;        /* Timer IRQ Enable Register 0x00 */
    uint32_t tmr_irq_sta_reg;       /* Timer Status Register  0x04 */
    uint32_t tmr_reserved01[2];
    uint32_t tmr0_ctrl_reg;         /* Timer 0 Control Register  0x10 */
    uint32_t tmr0_intv_value_reg;   /* Timer 0 Interval Value Register 0x14 */
    uint32_t tmr0_cur_value_reg;    /* Timer 0 Current Value Register  0x18 */
};
typedef volatile struct timer timer_t;
extern timer_t *timer;

static inline void resetTimer(void)
{
    timer->tmr_irq_sta_reg = TMR0_IRQ_PEND_FLAG;
}

