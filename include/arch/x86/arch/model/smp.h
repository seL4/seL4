/*
 * Copyright 2016, Data61
 * Commonwealth Scientific and Industrial Research Organisation (CSIRO)
 * ABN 41 687 119 230.
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(D61_GPL)
 */

#pragma once

#include <config.h>
#include <arch/types.h>
#include <model/statedata.h>

#if CONFIG_MAX_NUM_NODES > 1
typedef struct smpStatedata {
    nodeState_t CPU;
    char padding[CONFIG_CACHE_LN_SZ - (sizeof(nodeState_t) % CONFIG_CACHE_LN_SZ)];
} smpStatedata_t;

typedef struct cpu_id_mapping {
    /* translate actual cpu index to APIC id */
    cpu_id_t index_to_cpu_id[CONFIG_MAX_NUM_NODES];
} cpu_id_mapping_t;

extern smpStatedata_t ksSMP[CONFIG_MAX_NUM_NODES] VISIBLE;
extern cpu_id_mapping_t cpu_mapping;

#define NODE_STATE(_state)        ksSMP[getCurrentCPUIndex()].CPU._state
#define ARCH_NODE_STATE(_state)   ksSMP[getCurrentCPUIndex()].CPU.arch._state
#define MODE_NODE_STATE(_state)   ksSMP[getCurrentCPUIndex()].CPU.arch.mode._state

#else

#define NODE_STATE(_state)        _state
#define ARCH_NODE_STATE(_state)   _state
#define MODE_NODE_STATE(_state)   _state

#endif /* CONFIG_MAX_NUM_NODES */
