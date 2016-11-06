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

#ifndef __MODE_MODEL_SMP_H_
#define __MODE_MODEL_SMP_H_

#include <config.h>

#if CONFIG_MAX_NUM_NODES > 1

typedef struct nodeInfo {
    void *stackTop;
    cpu_id_t index;
} nodeInfo_t;

extern nodeInfo_t node_info[CONFIG_MAX_NUM_NODES];

static inline PURE cpu_id_t getCurrentCPUIndex(void)
{
    cpu_id_t index;
    asm volatile("movq %%gs:%c[offset], %[result]"
        : [result] "=r" (index)
        : [offset] "i" (OFFSETOF(nodeInfo_t, index)));
    return index;
}

#endif /* CONFIG_MAX_NUM_NODES > 1 */

#endif /* __MODE_MODEL_SMP_H_ */
