/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
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

#define SYSTEM_WRITE_WORD(reg, v) MSR(reg, v)
#define SYSTEM_READ_WORD(reg, v)  MRS(reg, v)
#define SYSTEM_WRITE_64(reg, v)   MSR(reg, v)
#define SYSTEM_READ_64(reg, v)    MRS(reg, v)

