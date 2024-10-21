/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 * Copyright 2024, Capabilities Limited
 * CHERI support contributed by Capabilities Limited was developed by Hesham Almatary
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#pragma once

static inline void wfi(void)
{
    asm volatile("wfi" ::: "memory");
}

static inline void dsb(void)
{
    asm volatile("dsb sy" ::: "memory");
}

static inline void dsb_ishst(void)
{
    asm volatile("dsb ishst" ::: "memory");
}

static inline void dmb(void)
{
    asm volatile("dmb sy" ::: "memory");
}

static inline void isb(void)
{
    asm volatile("isb sy" ::: "memory");
}

#define MRS(reg, v)  asm volatile("mrs %x0," reg : "=r"(v))
#define MSR(reg, v)                                \
    do {                                           \
        word_t _v = v;                             \
        asm volatile("msr " reg ",%x0" :: "r" (_v));\
    }while(0)
#define CMRS(reg, v)  asm volatile("mrs %0," reg : "=C"(v))
#define CMSR(reg, v)                               \
    do {                                           \
        register_t _v = v;                         \
        asm volatile("msr " reg ",%0" :: "C" (_v));\
    }while(0)

#if defined(__CHERI_PURE_CAPABILITY__)
    #define PMRS(reg, v) CMRS("c"reg, v)
    #define PMSR(reg, v) CMSR("c"reg, v)
#else
    #define PMRS(reg, v) MRS(reg, v)
    #define PMSR(reg, v) MSR(reg, v)
#endif

#define SYSTEM_WRITE_WORD(reg, v) MSR(reg, v)
#define SYSTEM_READ_WORD(reg, v)  MRS(reg, v)
#define SYSTEM_WRITE_64(reg, v)   MSR(reg, v)
#define SYSTEM_READ_64(reg, v)    MRS(reg, v)

