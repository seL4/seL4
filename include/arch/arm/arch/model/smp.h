/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#pragma once

#include <config.h>
#include <mode/smp/smp.h>
#include <model/smp.h>

#ifdef ENABLE_SMP_SUPPORT
static inline cpu_id_t cpuIndexToID(word_t index)
{
    return BIT(index);
}
#endif /* ENABLE_SMP_SUPPORT */

