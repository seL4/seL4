/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#pragma once

static inline void wfi(void)
{
    /*
     * The wait-for-interrupt doesn't work on the KZM board, although,
     * according to the arm infocenter, it should. With the KZM currently
     * being the only supported ARMv6 platform, it is unclear at this
     * time wether it works for other SoCs (e.g. BCM2835), so we explicitly
     * disable only the KZM here.
     *
     * See: http://infocenter.arm.com/help/index.jsp?topic=/com.arm.doc.faqs/ka13332.html
     */
#ifndef CONFIG_PLAT_KZM
    asm volatile("mcr p15, 0, %0, c7, c0, 4" : : "r"(0) : "memory");
#endif
}

static inline void dsb(void)
{
    asm volatile("mcr p15, 0, %0, c7, c10, 4" : : "r"(0) : "memory");
}

static inline void dmb(void)
{
    asm volatile("mcr p15, 0, %0, c7, c10, 5" : : "r"(0) : "memory");
}

static inline void isb(void)
{
    asm volatile("mcr p15, 0, %0, c7, c5, 4" : : "r"(0) : "memory");
}

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

