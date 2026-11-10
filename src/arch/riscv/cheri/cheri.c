/*
 * Copyright (c) 2025, Capabilities Limited
 * CHERI support contributed by Capabilities Limited was developed by Hesham Almatary
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */

#if defined(CONFIG_HAVE_CHERI)
#include <cheri/cheri.h>

void *__capability CheriArch_BuildCap(void *__capability src, word_t base, word_t addr, word_t size,
                                      CheriCapMeta_t meta, int user)
{
    return cheri_sel4_build_cap(src,                        /* src */
                                base,                       /* base */
                                addr,                       /* address */
                                size,                       /* size */
                                CheriCapMeta_get_AP(meta),  /* perms */
                                CheriCapMeta_get_M(meta),   /* flags */
                                CheriCapMeta_get_CT(meta),  /* sentry */
                                user);                      /* user */
}

CheriCapMeta_t CheriArch_GetCapMeta(void *__capability src)
{
    return CheriCapMeta_new(
               __builtin_cheri_perms_get(src),
               __builtin_cheri_type_get(src),
               __builtin_cheri_flags_get(src),
               __builtin_cheri_tag_get(src)
           );
}
#endif
