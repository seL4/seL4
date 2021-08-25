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


NODE_STATE_BEGIN(archNodeState)
/* TODO: add RISCV-dependent fields here */
/* Bitmask of all cores should receive the reschedule IPI */
NODE_STATE_DECLARE(word_t, ipiReschedulePending);
NODE_STATE_END(archNodeState);

extern asid_pool_t *riscvKSASIDTable[BIT(asidHighBits)];

/* Kernel Page Tables */
extern pte_t kernel_root_pageTable[BIT(PT_INDEX_BITS)] VISIBLE;

/* We need to introduce a level2 pagetable in order to map OpenSBI to a separate
 * page entry to avoid PMP exception. */
#if __riscv_xlen != 32
extern pte_t kernel_image_level2_pt[BIT(PT_INDEX_BITS)];
extern pte_t kernel_image_level2_dev_pt[BIT(PT_INDEX_BITS)];
#elif defined(CONFIG_KERNEL_LOG_BUFFER)
extern pte_t kernel_image_level2_log_buffer_pt[BIT(PT_INDEX_BITS)];
#endif

