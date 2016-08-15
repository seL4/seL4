/*
 * Copyright 2016, NICTA
 *
 * This software may be distributed and modified according to the terms of
 * the BSD 2-Clause license. Note that NO WARRANTY is provided.
 * See "LICENSE_BSD2.txt" for details.
 *
 * @TAG(NICTA_BSD)
 */

#pragma once

/* Wrapper around the cap rights interface exposing the rt cap rights interface.
 * This is a stop gap until cap rights becomes a shared type, and should be
 * removed at that point.
 */

#include <sel4/types.h>

/* Additional sets of cap rights not defined by libsel4 */
#define seL4_NoWrite   seL4_CapRights_new(true, true, false)
#define seL4_ReadWrite seL4_CapRights_new(false, true, true)

typedef seL4_CapRights seL4_CapRights_t;

/* Returns a set of cap rights based on its arguments.
 * Non-zero values in arguments will result in that right being
 * present in the result. */
static inline seL4_CapRights_t CONST
seL4_CapRights_new(seL4_Uint32 grant, seL4_Uint32 read, seL4_Uint32 write)
{
    seL4_CapRights cap_rights = 0;

    /* Ensure arguments are either 0 or 1 */
    seL4_DebugAssert(grant <= 1);
    seL4_DebugAssert(read <= 1);
    seL4_DebugAssert(write <= 1);

    if (grant) {
        cap_rights |= seL4_CanGrant;
    }
    if (read) {
        cap_rights |= seL4_CanRead;
    }
    if (write) {
        cap_rights |= seL4_CanWrite;
    }

    return cap_rights;
}

static inline seL4_Uint32 CONST
seL4_CapRights_get_capAllowGrant(seL4_CapRights_t cap_rights)
{
    return !!(cap_rights & seL4_CanGrant);
}

static inline seL4_Uint32 CONST
seL4_CapRights_get_capAllowRead(seL4_CapRights_t cap_rights)
{
    return !!(cap_rights & seL4_CanRead);
}

static inline seL4_Uint32 CONST
seL4_CapRights_get_capAllowWrite(seL4_CapRights_t cap_rights)
{
    return !!(cap_rights & seL4_CanWrite);
}

static inline seL4_CapRights_t CONST
seL4_CapRights_set_capAllowGrant(seL4_CapRights_t cap_rights, seL4_Uint32 value)
{
    /* Ensure value is either 0 or 1 */
    seL4_DebugAssert(value <= 1);

    if (value) {
        return cap_rights | seL4_CanGrant;
    } else {
        return cap_rights & ~seL4_CanGrant;
    }
}

static inline seL4_CapRights_t CONST
seL4_CapRights_set_capAllowRead(seL4_CapRights_t cap_rights, seL4_Uint32 value)
{
    /* Ensure value is either 0 or 1 */
    seL4_DebugAssert(value <= 1);

    if (value) {
        return cap_rights | seL4_CanRead;
    } else {
        return cap_rights & ~seL4_CanRead;
    }
}

static inline seL4_CapRights_t CONST
seL4_CapRights_set_capAllowWrite(seL4_CapRights_t cap_rights, seL4_Uint32 value)
{
    /* Ensure value is either 0 or 1 */
    seL4_DebugAssert(value <= 1);

    if (value) {
        return cap_rights | seL4_CanWrite;
    } else {
        return cap_rights & ~seL4_CanWrite;
    }
}
