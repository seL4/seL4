/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(GD_GPL)
 */

#ifndef __TYPES_H
#define __TYPES_H

#include <stdint.h>
#include <api/types.h>
#include <object/structures.h>
#include <arch/types.h>

enum _bool {
    false = 0,
    true  = 1
};
typedef uint32_t bool_t;

typedef struct region {
    pptr_t start;
    pptr_t end;
} region_t;

typedef struct p_region {
    paddr_t start;
    paddr_t end;
} p_region_t;

typedef struct v_region {
    vptr_t start;
    vptr_t end;
} v_region_t;

#define REG_EMPTY (region_t){ .start = 0, .end = 0 }
#define P_REG_EMPTY (p_region_t){ .start = 0, .end = 0 }

struct pde_range {
    pde_t *base;
    unsigned int length;
};
typedef struct pde_range pde_range_t;

struct pte_range {
    pte_t *base;
    unsigned int length;
};
typedef struct pte_range pte_range_t;

typedef cte_t *cte_ptr_t;

struct extra_caps {
    cte_ptr_t excaprefs[seL4_MsgMaxExtraCaps];
};
typedef struct extra_caps extra_caps_t;

#endif
