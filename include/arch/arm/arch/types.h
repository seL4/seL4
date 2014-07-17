/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(GD_GPL)
 */

#ifndef __ARCH_TYPES_H
#define __ARCH_TYPES_H

#include <stdint.h>

typedef uint32_t word_t;
typedef uint32_t vptr_t;
typedef uint32_t paddr_t;
typedef uint32_t pptr_t;

typedef uint32_t node_id_t;

typedef uint8_t  hw_asid_t;

enum hwASIDConstants {
    hwASIDMax = 255,
    hwASIDBits = 8
};

#endif
