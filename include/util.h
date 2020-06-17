/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#pragma once

#define MASK(n) (BIT(n)-UL_CONST(1))
#define IS_ALIGNED(n, b) (!((n) & MASK(b)))
#define ROUND_DOWN(n, b) (((n) >> (b)) << (b))
#define ROUND_UP(n, b) (((((n) - UL_CONST(1)) >> (b)) + UL_CONST(1)) << (b))
#define ARRAY_SIZE(x) (sizeof(x) / sizeof(x[0]))
#define MIN(a,b) (((a)<(b))?(a):(b))
#define MAX(a,b) (((a)>(b))?(a):(b))
#define PASTE(a, b) a ## b
#define _STRINGIFY(a) #a
#define STRINGIFY(a) _STRINGIFY(a)

/* time constants */
#define MS_IN_S     1000llu
#define US_IN_MS    1000llu
#define HZ_IN_KHZ   1000llu
#define KHZ_IN_MHZ  1000llu
#define HZ_IN_MHZ   1000000llu

#ifndef __ASSEMBLER__

#define NULL ((void *)0)
#define BIT(n) (1ul << (n))
#define UL_CONST(x) PASTE(x, ul)

#define PACKED       __attribute__((packed))
#define NORETURN     __attribute__((__noreturn__))
#define CONST        __attribute__((__const__))
#define PURE         __attribute__((__pure__))
#define ALIGN(n)     __attribute__((__aligned__(n)))
#define FASTCALL     __attribute__((fastcall))
#ifdef __clang__
#define VISIBLE      /* nothing */
#else
#define VISIBLE      __attribute__((externally_visible))
#endif
#define NO_INLINE    __attribute__((noinline))
#define FORCE_INLINE __attribute__((always_inline))
#define SECTION(sec) __attribute__((__section__(sec)))
#define UNUSED       __attribute__((unused))
#define USED         __attribute__((used))
#define FASTCALL     __attribute__((fastcall))
#ifdef __clang__
#define FORCE_O2     /* nothing */
#else
#define FORCE_O2     __attribute__((optimize("O2")))
#endif
/** MODIFIES: */
void __builtin_unreachable(void);
#define UNREACHABLE()  __builtin_unreachable()
#define MAY_ALIAS    __attribute__((may_alias))

#define OFFSETOF(type, member) \
    __builtin_offsetof(type, member)

#ifdef __GNUC__
/* Borrowed from linux/include/linux/compiler.h */
#define likely(x)   __builtin_expect(!!(x), 1)
#define unlikely(x) __builtin_expect(!!(x), 0)
#else
#define likely(x)   (!!(x))
#define unlikely(x) (!!(x))
#endif

/* need that for compiling with c99 instead of gnu99 */
#define asm __asm__

/* Evaluate a Kconfig-provided configuration setting at compile-time. */
#define config_set(macro) _is_set_(macro)
#define _macrotest_1 ,
#define _is_set_(value) _is_set__(_macrotest_##value)
#define _is_set__(comma) _is_set___(comma 1, 0)
#define _is_set___(_, v, ...) v

/* Check the existence of a configuration setting, returning one value if it
 * exists and a different one if it does not */
#define config_ternary(macro, true, false) _config_ternary(macro, true, false)
#define _config_ternary(value, true, false) _config_ternary_(_macrotest_##value, true, false)
#define _config_ternary_(comma, true, false) _config_ternary__(comma true, false)
#define _config_ternary__(_, v, ...) v

/** MODIFIES:
    FNSPEC
        halt_spec: "\<Gamma> \<turnstile> {} Call halt_'proc {}"
*/
void halt(void) NORETURN;
void memzero(void *s, unsigned long n);
void *memset(void *s, unsigned long c, unsigned long n) VISIBLE;
void *memcpy(void *ptr_dst, const void *ptr_src, unsigned long n) VISIBLE;
int PURE strncmp(const char *s1, const char *s2, int n);
long CONST char_to_long(char c);
long PURE str_to_long(const char *str);


int __builtin_clzl(unsigned long x);
int __builtin_ctzl(unsigned long x);

#ifdef CONFIG_ARCH_RISCV
uint32_t __clzsi2(uint32_t x);
uint32_t __ctzsi2(uint32_t x);
uint32_t __clzdi2(uint64_t x);
uint32_t __ctzdi2(uint64_t x);
#endif
/** MODIFIES: */
/** DONT_TRANSLATE */
/** FNSPEC clzl_spec:
  "\<forall>s. \<Gamma> \<turnstile>
    {\<sigma>. s = \<sigma> \<and> x_' s \<noteq> 0 }
      \<acute>ret__long :== PROC clzl(\<acute>x)
    \<lbrace> \<acute>ret__long = of_nat (word_clz (x_' s)) \<rbrace>"
*/
static inline long
CONST clzl(unsigned long x)
{
    return __builtin_clzl(x);
}

/** MODIFIES: */
/** DONT_TRANSLATE */
/** FNSPEC ctzl_spec:
  "\<forall>s. \<Gamma> \<turnstile>
    {\<sigma>. s = \<sigma> \<and> x_' s \<noteq> 0 }
      \<acute>ret__long :== PROC ctzl(\<acute>x)
    \<lbrace> \<acute>ret__long = of_nat (word_ctz (x_' s)) \<rbrace>"
*/
static inline long
CONST ctzl(unsigned long x)
{
    return __builtin_ctzl(x);
}

#define CTZL(x) __builtin_ctzl(x)

int __builtin_popcountl(unsigned long x);

/** DONT_TRANSLATE */
/** FNSPEC clzll_spec:
  "\<forall>s. \<Gamma> \<turnstile>
    {\<sigma>. s = \<sigma> \<and> x_' s \<noteq> 0 }
      \<acute>ret__longlong :== PROC clzll(\<acute>x)
    \<lbrace> \<acute>ret__longlong = of_nat (word_clz (x_' s)) \<rbrace>"
*/
static inline long long CONST clzll(unsigned long long x)
{
    return __builtin_clzll(x);
}

/** DONT_TRANSLATE */
static inline long
CONST popcountl(unsigned long mask)
{
#ifndef __POPCNT__
    unsigned int count; // c accumulates the total bits set in v
    for (count = 0; mask; count++) {
        mask &= mask - 1; // clear the least significant bit set
    }

    return count;
#else
    return __builtin_popcountl(mask);
#endif
}

#define POPCOUNTL(x) popcountl(x)

/* Can be used to insert padding to the next L1 cache line boundary */
#define PAD_TO_NEXT_CACHE_LN(used) char padding[L1_CACHE_LINE_SIZE - ((used) % L1_CACHE_LINE_SIZE)]

#else /* __ASSEMBLER__ */

/* Some assemblers don't recognise ul (unsigned long) suffix */
#define BIT(n) (1 << (n))
#define UL_CONST(x) x

#endif /* !__ASSEMBLER__ */

