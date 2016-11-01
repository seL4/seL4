/*
 * Copyright 2019, Data61
 * Commonwealth Scientific and Industrial Research Organisation (CSIRO)
 * ABN 41 687 119 230.
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(DATA61_GPL)
 */
#ifndef __MODE_UTIL_H
#define __MODE_UTIL_H

#include <config.h>
#include <types.h>
#include <arch/model/statedata.h>

static inline CONST uint64_t div64(uint64_t numerator, uint32_t denominator)
{
    return numerator / denominator;
}

#endif /* __MODE_UTIL_H */
