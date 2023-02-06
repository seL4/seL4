
/*
 * Copyright 2022, tyyteam(Qingtao Liu, Yang Lei, Yang Chen)
 * qtliu@mail.ustc.edu.cn, le24@mail.ustc.edu.cn, chenyangcs@mail.ustc.edu.cn
 *
 * Derived from:
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
/* TODO: add Loongarch-dependent fields here */
/* Bitmask of all cores should receive the reschedule IPI */
NODE_STATE_DECLARE(word_t, ipiReschedulePending);
NODE_STATE_END(archNodeState);

extern asid_pool_t *loongarchKSASIDTable[BIT(asidHighBits)];

/* Kernel Page Tables */
extern pte_t kernel_l1pt[BIT(PT_INDEX_BITS)] ALIGN_BSS(BIT(seL4_PageTableBits));
extern pte_t kernel_l2pt[BIT(PT_INDEX_BITS)][BIT(PT_INDEX_BITS)] ALIGN_BSS(BIT(seL4_PageTableBits));

extern pte_t kernel_image_pt[BIT(PT_INDEX_BITS)] ALIGN_BSS(BIT(seL4_PageTableBits));
extern pte_t kernel_devices_pt[BIT(PT_INDEX_BITS)] ALIGN_BSS(BIT(seL4_PageTableBits));

// #elif defined(CONFIG_KERNEL_LOG_BUFFER)
// extern pte_t kernel_image_level2_log_buffer_pt[BIT(PT_INDEX_BITS)];
//#endif

