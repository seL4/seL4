/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(GD_GPL)
 */

#ifndef __ARCH_API_TYPES_H
#define __ARCH_API_TYPES_H

#include <api/objecttype.h>
#include <arch/api/objecttype.h>

#define pageType SmallPageObject

enum asidConstants {
    asidInvalid = 0
};

#define asidMax (BIT(asidLowBits+asidHighBits)-1)

typedef uint32_t asid_t;

#endif
