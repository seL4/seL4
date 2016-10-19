/*
 * Copyright 2016, NICTA
 *
 * This software may be distributed and modified according to the terms of
 * the BSD 2-Clause license. Note that NO WARRANTY is provided.
 * See "LICENSE_BSD2.txt" for details.
 *
 * @TAG(NICTA_BSD)
 */

#ifndef __LIBSEL4_SEL4_CAP_RIGHTS_H_
#define __LIBSEL4_SEL4_CAP_RIGHTS_H_

/* Future interface to cap rights that wraps existing interface.
 * This wrapper can be removed when the future interface is adopted.
 */

#include <sel4/types.h>

typedef seL4_CapRights seL4_CapRights_t;

/* Returns a set of cap rights based on its arguments.
 * Non-zero values in arguments will result in that right being
 * present in the result. */
static inline seL4_CapRights_t CONST
seL4_CapRights_new(seL4_Uint32 grant, seL4_Uint32 read, seL4_Uint32 write)
{
    unsigned cap_rights = 0;

    /* Ensure arguments are either 0 or 1 */
    seL4_DebugAssert(grant <= 1);
    seL4_DebugAssert(read <= 1);
    seL4_DebugAssert(write <= 1);

    if (grant) {
        cap_rights |= (unsigned)seL4_CanGrant;
    }
    if (read) {
        cap_rights |= (unsigned)seL4_CanRead;
    }
    if (write) {
        cap_rights |= (unsigned)seL4_CanWrite;
    }

    return (seL4_CapRights_t)cap_rights;
}

static inline seL4_Uint32 CONST
seL4_CapRights_get_capAllowGrant(seL4_CapRights_t cap_rights)
{
    return !!((unsigned)cap_rights & (unsigned)seL4_CanGrant);
}

static inline seL4_Uint32 CONST
seL4_CapRights_get_capAllowRead(seL4_CapRights_t cap_rights)
{
    return !!((unsigned)cap_rights & (unsigned)seL4_CanRead);
}

static inline seL4_Uint32 CONST
seL4_CapRights_get_capAllowWrite(seL4_CapRights_t cap_rights)
{
    return !!((unsigned)cap_rights & (unsigned)seL4_CanWrite);
}

static inline seL4_CapRights_t CONST
seL4_CapRights_set_capAllowGrant(seL4_CapRights_t cap_rights, seL4_Uint32 value)
{
    /* Ensure value is either 0 or 1 */
    seL4_DebugAssert(value <= 1);

    if (value) {
        return (seL4_CapRights_t)((unsigned)cap_rights | (unsigned)seL4_CanGrant);
    } else {
        return (seL4_CapRights_t)((unsigned)cap_rights & ~(unsigned)seL4_CanGrant);
    }
}

static inline seL4_CapRights_t CONST
seL4_CapRights_set_capAllowRead(seL4_CapRights_t cap_rights, seL4_Uint32 value)
{
    /* Ensure value is either 0 or 1 */
    seL4_DebugAssert(value <= 1);

    if (value) {
        return (seL4_CapRights_t)((unsigned)cap_rights | (unsigned)seL4_CanRead);
    } else {
        return (seL4_CapRights_t)((unsigned)cap_rights & ~(unsigned)seL4_CanRead);
    }
}

static inline seL4_CapRights_t CONST
seL4_CapRights_set_capAllowWrite(seL4_CapRights_t cap_rights, seL4_Uint32 value)
{
    /* Ensure value is either 0 or 1 */
    seL4_DebugAssert(value <= 1);

    if (value) {
        return (seL4_CapRights_t)((unsigned)cap_rights | (unsigned)seL4_CanWrite);
    } else {
        return (seL4_CapRights_t)((unsigned)cap_rights & ~(unsigned)seL4_CanWrite);
    }
}

#endif /* __LIBSEL4_SEL4_CAP_RIGHTS_H_ */
