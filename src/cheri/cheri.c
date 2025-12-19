/*
 * Copyright (c) 2025, Capabilities Limited
 * CHERI support contributed by Capabilities Limited was developed by Hesham Almatary
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */

#if defined(CONFIG_HAVE_CHERI)
#include <cheri/cheri.h>

inline void *__capability cheri_sel4_build_cap(void *__capability src, word_t base, word_t address, word_t size,
                                               word_t perms, word_t flags, int sentry, int user)
{
    void *__capability returned_cap = src;

    returned_cap = __builtin_cheri_perms_and(returned_cap, perms);
    returned_cap = __builtin_cheri_address_set(returned_cap, base);
    returned_cap = __builtin_cheri_bounds_set(returned_cap, size);
    returned_cap = __builtin_cheri_address_set(returned_cap, address);
    returned_cap = __builtin_cheri_flags_set(returned_cap, flags);

    if (user) {
        returned_cap = __builtin_cheri_perms_and(returned_cap, ~__CHERI_CAP_PERMISSION_ACCESS_SYSTEM_REGISTERS__);
    }

    if (sentry) {
        returned_cap = __builtin_cheri_seal_entry(returned_cap);
    }

    return returned_cap;
}
#endif
