/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#pragma once

#include <config.h>
#include <arch/types.h>
#include <arch/model/statedata.h>
#include <model/statedata.h>
#include <model/smp.h>
#include <mode/model/smp.h>

#ifdef ENABLE_SMP_SUPPORT

typedef struct cpu_id_mapping {
    cpu_id_t index_to_cpu_id[CONFIG_MAX_NUM_NODES];

#ifdef CONFIG_USE_LOGICAL_IDS
    logical_id_t index_to_logical_id[CONFIG_MAX_NUM_NODES];
    word_t other_indexes_in_cluster[CONFIG_MAX_NUM_NODES];
#endif /* CONFIG_USE_LOGICAL_IDS */
} cpu_id_mapping_t;

extern cpu_id_mapping_t cpu_mapping;

static inline cpu_id_t cpuIndexToID(word_t index)
{
    return cpu_mapping.index_to_cpu_id[index];
}

static inline PURE word_t getCurrentCPUID(void)
{
    return cpu_mapping.index_to_cpu_id[getCurrentCPUIndex()];
}

static inline bool_t try_arch_atomic_exchange_rlx(void *ptr, void *new_val, void **prev)
{
    *prev = __atomic_exchange_n((void **) ptr, new_val, __ATOMIC_RELAXED);
    return true;
}

#endif /* ENABLE_SMP_SUPPORT */
