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

#ifndef __PLAT_MACHINE_TIMER_H
#define __PLAT_MACHINE_TIMER_H

#include <config.h>
#include <basic_types.h>
#include <arch/linker.h>

#define TIMER_CLOCK_HZ 24000000ULL
/* see tools/reciprocal.py for calculation of this value */
#define CLK_MAGIC 2863311531
#define CLK_SHIFT 36
compile_assert(magic_will_work, TIMER_CLOCK_MHZ == 24llu)

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

static inline CONST time_t
getKernelWcetUs(void)
{
    fail("Not implemented");
}

static inline CONST ticks_t
getTimerPrecision(void)
{
    fail("Not implemented");
    return 0llu;
}

static inline ticks_t
getCurrentTime(void) {
    fail("Not implemented");
    return 0llu;
}

static inline void
setDeadline(ticks_t deadline) {
    fail("Not implemented");
}

static inline void
ackDeadlineIRQ(void) {
    fail("Not implemented");
}

#endif /* !__PLAT_MACHINE_TIMER_H */
