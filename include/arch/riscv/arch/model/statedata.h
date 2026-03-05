/*
 * Copyright 2020, DornerWorks
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 * Copyright 2015, 2016 Hesham Almatary <heshamelmatary@gmail.com>
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#pragma once

#include <config.h>
#include <types.h>
#include <util.h>
#include <model/statedata.h>
#include <object/structures.h>
#include <arch/types.h>
typedef word_t jmp_buf[16]; /* ra, sp, gp, tp, s0-s11 for setjmp/longjmp */

int setjmp(jmp_buf env);
void longjmp(jmp_buf env, int val);

/* Per-CPU state for safe user memory access. Maintained outside archNodeState
 * to preserve fixed assembly offsets in ksSMP[].cpu. */
typedef struct {
    bool_t in_progress;
    word_t saved_satp;
    word_t saved_sstatus;
    jmp_buf jmp_buf;
} riscv_user_access_state_t;
extern riscv_user_access_state_t riscv_user_access_state[CONFIG_MAX_NUM_NODES];

NODE_STATE_BEGIN(archNodeState)
/* TODO: add RISCV-dependent fields here */
/* Bitmask of all cores should receive the reschedule IPI */
NODE_STATE_DECLARE(word_t, ipiReschedulePending);
NODE_STATE_END(archNodeState);

extern asid_pool_t *riscvKSASIDTable[nASIDPools];

/* Kernel Page Tables */
extern pte_t kernel_root_pageTable[BIT(PT_INDEX_BITS)] VISIBLE;

/* We need to introduce a level 1 page table in order to map OpenSBI into
   a separate 2MiB page to avoid a PMP exception */
#if __riscv_xlen != 32
extern pte_t kernel_image_level1_pt[BIT(PT_INDEX_BITS)];
extern pte_t kernel_image_level1_dev_pt[BIT(PT_INDEX_BITS)];
#elif defined(CONFIG_KERNEL_LOG_BUFFER)
extern pte_t kernel_image_level1_log_buffer_pt[BIT(PT_INDEX_BITS)];
#endif
