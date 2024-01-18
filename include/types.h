/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#pragma once

#include <config.h>
#include <basic_types.h> /* includes stdint.h and arch/types.h */
#include <api/types.h>
#include <object/structures.h>

typedef struct pde_range {
    pde_t *base;
    word_t length;
} pde_range_t;

typedef struct pte_range {
    pte_t *base;
    word_t length;
} pte_range_t;

typedef cte_t *cte_ptr_t;

typedef struct extra_caps {
    cte_ptr_t excaprefs[seL4_MsgMaxExtraCaps];
} extra_caps_t;

typedef struct region {
    pptr_t start;
    pptr_t end;
} region_t;

#define REG_EMPTY ((region_t){ .start = 0, .end = 0 })

typedef struct p_region {
    paddr_t start;
    paddr_t end;
} p_region_t;

#define P_REG_EMPTY ((p_region_t){ .start = 0, .end = 0 })

typedef struct v_region {
    vptr_t start;
    vptr_t end;
} v_region_t;
