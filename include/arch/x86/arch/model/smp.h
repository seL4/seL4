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
#include <arch/model/statedata.h>
#include <model/statedata.h>

#if CONFIG_MAX_NUM_NODES > 1

/* Use this to avoid false sharing between cores for per-core data structures */
#define PAD_TO_NEXT_CACHE_LN(used) char padding[CONFIG_CACHE_LN_SZ - ((used) % CONFIG_CACHE_LN_SZ)]

typedef struct smpStatedata {
    archNodeState_t cpu;
    nodeState_t system;
    PAD_TO_NEXT_CACHE_LN(sizeof(archNodeState_t) + sizeof(nodeState_t));
} smpStatedata_t;

typedef struct cpu_id_mapping {
    cpu_id_t index_to_cpu_id[CONFIG_MAX_NUM_NODES];
} cpu_id_mapping_t;

extern smpStatedata_t ksSMP[CONFIG_MAX_NUM_NODES] VISIBLE;
extern cpu_id_mapping_t cpu_mapping;

#define ARCH_NODE_STATE(_state)             ksSMP[getCurrentCPUIndex()].cpu._state
#define MODE_NODE_STATE(_state)             ksSMP[getCurrentCPUIndex()].cpu.mode._state
#define NODE_STATE_ON_CORE(_state, _core)   ksSMP[(_core)].system._state

#else

#define ARCH_NODE_STATE(_state)             _state
#define MODE_NODE_STATE(_state)             _state
#define NODE_STATE_ON_CORE(_state, _core)   _state

#endif /* CONFIG_MAX_NUM_NODES */

#define NODE_STATE(_state)                  NODE_STATE_ON_CORE(_state, getCurrentCPUIndex())
