/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#pragma once

#define TMR_INTS_EVENT       BIT(0)

/* 32 bit down counter */
struct timer {
    uint32_t load;
    uint32_t count;
    uint32_t ctrl;
    uint32_t ints;
};
typedef volatile struct timer timer_t;
extern timer_t *priv_timer;

static inline void resetTimer(void)
{
    priv_timer->ints = TMR_INTS_EVENT;
}

