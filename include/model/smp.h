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

#ifndef __MODEL_SMP_H_
#define __MODEL_SMP_H_

#include <config.h>
#include <arch/types.h>
#include <arch/model/statedata.h>
#include <model/statedata.h>

#ifdef ENABLE_SMP_SUPPORT

typedef struct smpStatedata {
    archNodeState_t cpu;
    nodeState_t system;
    PAD_TO_NEXT_CACHE_LN(sizeof(archNodeState_t) + sizeof(nodeState_t));
} smpStatedata_t;

extern smpStatedata_t ksSMP[CONFIG_MAX_NUM_NODES];

void migrateTCB(tcb_t *tcb, word_t new_core);

#endif /* ENABLE_SMP_SUPPORT */

#endif /* __MODEL_SMP_H_ */
