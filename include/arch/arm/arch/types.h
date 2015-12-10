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

#include <assert.h>
#include <stdint.h>

compile_assert(long_is_32bits, sizeof(unsigned long) == 4)

typedef unsigned long word_t;
typedef word_t vptr_t;
typedef word_t paddr_t;
typedef word_t pptr_t;

typedef word_t node_id_t;

typedef uint8_t  hw_asid_t;

enum hwASIDConstants {
    hwASIDMax = 255,
    hwASIDBits = 8
};

#endif
