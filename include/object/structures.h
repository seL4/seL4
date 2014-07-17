/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(GD_GPL)
 */

#ifndef __OBJECT_STRUCTURES_H
#define __OBJECT_STRUCTURES_H

#include <api/types.h>
#include <stdint.h>

enum irq_state {
    IRQInactive  = 0,
    IRQNotifyAEP = 1,
    IRQTimer     = 2,
    IRQReserved  = 3,
};
typedef uint32_t irq_state_t;

typedef struct dschedule {
    dom_t domain;
    uint32_t length;
} dschedule_t;

#include <arch/object/structures.h>

#endif
