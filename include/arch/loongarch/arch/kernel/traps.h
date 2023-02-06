/*
 * Copyright 2022, tyyteam(Qingtao Liu, Yang Lei, Yang Chen)
 * qtliu@mail.ustc.edu.cn, le24@mail.ustc.edu.cn, chenyangcs@mail.ustc.edu.cn
 * 
 * Derived from:
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 * Copyright 2015, 2016 Hesham Almatary <heshamelmatary@gmail.com>
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#pragma once

#include <config.h>
#include <util.h>
#include <types.h>

/* This file contains some error handler statements*/

static inline void arch_c_entry_hook(void)
{
    //check CSR_PRMD_PPLV
    if ((r_csr_prmd() & CSR_PRMD_PPLV) != 3){
        fail("User images should run under privilege 3!\n");
    }

    //check CSR_CRMD_PLV 
    if((r_csr_crmd() & CSR_CRMD_PLV) != 0){
        fail("Trap should be handled under privilege 0!\n");
    }

    //check CSR_CRMD_IE
    if(intr_get() != 0){
        fail("Interrupts should be disabled in trap!\n");
    }
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
