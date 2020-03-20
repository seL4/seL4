/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#pragma once

#include <sel4/objecttype.h>
#include <config.h>
#include <sel4/sel4_arch/objecttype.h>
#include <sel4/arch/objecttype.h>

#define pageType PageObject4K

enum asidConstants {
    asidInvalid = 0
};

#define asidMax (BIT(asidLowBits + asidHighBits) - 1)

typedef word_t asid_t;
