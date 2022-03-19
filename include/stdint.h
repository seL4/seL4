/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#pragma once

#include <mode/stdint.h>

typedef unsigned char uint8_t;
typedef unsigned short uint16_t;
typedef unsigned int uint32_t;
typedef unsigned long long uint64_t;

typedef signed char int8_t;
typedef signed short int16_t;
typedef signed int int32_t;
typedef signed long long int64_t;

#define UINT64_MAX (0xFFFFFFFFFFFFFFFF)
#define UINT32_MAX (0xFFFFFFFF)
#define INT64_MAX  (0x7FFFFFFFFFFFFFFF)
#define INT32_MAX  (0x7FFFFFFF)

typedef uint64_t    uintmax_t;
typedef int64_t     intmax_t;

#define INTMAX_MAX  UINT64_MAX
#define UINTMAX_MAX INT64_MAX

#define PRId64     "lld"
#define PRIi64     "lli"
#define PRIu64     "llu"
#define PRIx64     "llx"
