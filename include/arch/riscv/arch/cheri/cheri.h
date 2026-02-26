/*
 * Copyright 2025, Capabilities Limited
 * CHERI support contributed by Capabilities Limited was developed by Hesham Almatary
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */
#pragma once

#define CHERI_CAP_MODE 0
#define CHERI_INT_MODE 1

#define CSR_SENVCFG_CRE_SHIFT 28

void *__capability CheriArch_BuildCap(void *__capability src, word_t base, word_t addr, word_t size,
                                      CheriCapMeta_t meta, int user);
CheriCapMeta_t CheriArch_GetCapMeta(void *__capability src);

static inline int CheriArch_isIntegerMode(void *__capability cap)
{
    return (__builtin_cheri_flags_get(cap) == CHERI_INT_MODE);
}

static inline void CheriArch_init_user(void)
{
    /* Enable CHERI instructions in user mode (senvcfg.CRE) */
    asm volatile("csrs senvcfg, %0"::"r"(BIT(CSR_SENVCFG_CRE_SHIFT)));
}

static inline void *__capability CheriArch_get_pcc(void)
{
    void *__capability pcc;
    /* cheriTODO: user a compiler builtin once supported */
    asm volatile("modesw.cap       \n"
                 ".option push     \n"
                 ".option capmode  \n"
                 "auipc %0, 0      \n"
                 ".option pop      \n"
                 "modesw.int       \n"
                 :"=C"(pcc)::);
    return pcc;
}
