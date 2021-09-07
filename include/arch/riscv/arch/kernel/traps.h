/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 * Copyright 2015, 2016 Hesham Almatary <heshamelmatary@gmail.com>
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#pragma once

#include <config.h>
#include <util.h>

static inline void arch_c_entry_hook(void)
{
    /* Nothing architecture specific to be done. */
}

static inline void arch_c_exit_hook(void)
{
    /* Nothing architecture specific to be done. */
}

#ifdef CONFIG_KERNEL_MCS
void c_handle_fastpath_reply_recv(word_t cptr, word_t msgInfo, word_t reply)
#else
void c_handle_fastpath_reply_recv(word_t cptr, word_t msgInfo)
#endif
VISIBLE NORETURN;

void c_handle_fastpath_call(word_t cptr, word_t msgInfo)
VISIBLE NORETURN;

void c_handle_syscall(word_t cptr, word_t msgInfo, syscall_t syscall)
VISIBLE NORETURN;

void c_handle_interrupt(void)
VISIBLE NORETURN;

void c_handle_exception(void)
VISIBLE NORETURN;

void restore_user_context(void)
VISIBLE NORETURN;

void handle_exception(void);

