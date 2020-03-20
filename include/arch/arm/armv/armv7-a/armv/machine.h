/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#pragma once

#include <util.h>

/* See idle_thread for an explanation as to why FORCE_INLINE is required here. */
static inline void FORCE_INLINE wfi(void)
{
    asm volatile("wfi" ::: "memory");
}

static inline void dsb(void)
{
    asm volatile("dsb" ::: "memory");
}

static inline void dmb(void)
{
    asm volatile("dmb" ::: "memory");
}

static inline void isb(void)
{
    asm volatile("isb" ::: "memory");
}

void lockTLBEntryCritical(unsigned int addr, unsigned int x, unsigned int y);

#define MRC(cpreg, v)  asm volatile("mrc  " cpreg :  "=r"(v))
#define MRRC(cpreg, v) asm volatile("mrrc " cpreg :  "=r"(v))
#define MCR(cpreg, v)                               \
    do {                                            \
        word_t _v = v;                            \
        asm volatile("mcr  " cpreg :: "r" (_v));    \
    }while(0)
#define MCRR(cpreg, v)                              \
    do {                                            \
        uint64_t _v = v;                            \
        asm volatile("mcrr " cpreg :: "r" (_v));    \
    }while(0)

#define SYSTEM_WRITE_WORD(reg, v) MCR(reg, v)
#define SYSTEM_READ_WORD(reg, v)  MRC(reg, v)
#define SYSTEM_WRITE_64(reg, v)  MCRR(reg, v)
#define SYSTEM_READ_64(reg, v)   MRRC(reg, v)
