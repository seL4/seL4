/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#ifndef __ARCH_API_TYPES_H
#define __ARCH_API_TYPES_H

#include <sel4/objecttype.h>
#include <sel4/sel4_arch/objecttype.h>
#include <sel4/arch/objecttype.h>

#define pageType SmallPageObject

enum asidConstants {
    asidInvalid = 0
};

#define asidMax (BIT(asidLowBits+asidHighBits)-1)

typedef word_t asid_t;

#endif
