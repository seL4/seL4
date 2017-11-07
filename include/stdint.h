/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(GD_GPL)
 */

#ifndef __STDINT_H
#define __STDINT_H

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

#endif
