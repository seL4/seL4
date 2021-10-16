/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#pragma once

#define va_start(v,l) __builtin_va_start(v,l)
#define va_copy(d,s) __builtin_va_copy(d,s)
#define va_end(v) __builtin_va_end(v)
#define va_arg(v,l) __builtin_va_arg(v,l)
typedef __builtin_va_list va_list;
