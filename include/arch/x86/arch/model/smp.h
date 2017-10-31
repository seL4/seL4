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

#ifndef __ARCH_MODEL_SMP_H_
#define __ARCH_MODEL_SMP_H_

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

static inline bool_t
try_arch_atomic_exchange(void *ptr, void *new_val, void **prev, int success_memorder, int failure_memorder)
{
    *prev = __atomic_exchange_n((void **) ptr, new_val, success_memorder);
    return true;
}

#endif /* ENABLE_SMP_SUPPORT */
#endif /* __ARCH_MODEL_SMP_H_ */
