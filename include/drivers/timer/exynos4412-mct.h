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

#ifndef __DRIVER_TIMER_MCT_EXYNOS4412_H
#define __DRIVER_TIMER_MCT_EXYNOS4412_H

#include <drivers/timer/mct.h>

static inline void resetTimer(void)
{
    mct_reset();
}

#endif /* !__DRIVER_TIMER_MCT_EXYNOS4412_H */
