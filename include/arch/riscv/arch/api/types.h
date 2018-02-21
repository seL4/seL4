/*
 * Copyright 2018, Data61
 * Commonwealth Scientific and Industrial Research Organisation (CSIRO)
 * ABN 41 687 119 230.
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(DATA61_GPL)
 */

/*
 *
 */

#ifndef __ARCH_API_TYPES
#define __ARCH_API_TYPES

#include <config.h>
#include <api/objecttype.h>
#include <mode/api/objecttype.h>
#include <arch/api/objecttype.h>


#define pageType PageObject4K

enum asidConstants {
    asidInvalid = 0
};

#define asidMax (BIT(asidLowBits + asidHighBits) - 1)

typedef word_t asid_t;

#endif
