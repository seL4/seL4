/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(GD_GPL)
 */

#ifndef __COMPOUND_TYPES_H
#define __COMPOUND_TYPES_H

#include <stdint.h>
#include <api/types.h>
#include <object/structures.h>
#include <arch/types.h>

struct pde_range {
    pde_t *base;
    word_t length;
};
typedef struct pde_range pde_range_t;

struct pte_range {
    pte_t *base;
    word_t length;
};
typedef struct pte_range pte_range_t;

typedef cte_t *cte_ptr_t;

struct extra_caps {
    cte_ptr_t excaprefs[seL4_MsgMaxExtraCaps];
};
typedef struct extra_caps extra_caps_t;

#endif /* __COMPOUND_TYPES_H */
