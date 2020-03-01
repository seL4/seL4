/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#ifndef __ARCH_FASTPATH_H
#define __ARCH_FASTPATH_H

#include <linker.h>
#include <mode/fastpath/fastpath.h>
#include <benchmark/benchmark_track.h>
#include <arch/machine/debug.h>

void slowpath(syscall_t syscall)
NORETURN;

void fastpath_call(word_t cptr, word_t r_msgInfo)
NORETURN SECTION(".vectors.fastpath_call");

#ifdef CONFIG_KERNEL_MCS
void fastpath_reply_recv(word_t cptr, word_t r_msgInfo, word_t reply)
#else
void fastpath_reply_recv(word_t cptr, word_t r_msgInfo)
#endif
NORETURN SECTION(".vectors.fastpath_reply_recv");

#endif /* __ARCH_FASTPATH_H */

