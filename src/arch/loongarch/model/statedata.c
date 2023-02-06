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

#include <util.h>
#include <api/types.h>
#include <arch/types.h>
#include <arch/model/statedata.h>
#include <arch/object/structures.h>
#include <linker.h>
#include <plat/machine/hardware.h>

/* The top level asid mapping table */
asid_pool_t *loongarchKSASIDTable[BIT(asidHighBits)];

/* Kernel Page Tables */
pte_t kernel_l1pt[BIT(PT_INDEX_BITS)] ALIGN_BSS(BIT(seL4_PageTableBits));
pte_t kernel_l2pt[BIT(PT_INDEX_BITS)][BIT(PT_INDEX_BITS)] ALIGN_BSS(BIT(seL4_PageTableBits));
// pte_t kernel_l3pt[4096 * BIT(PT_INDEX_BITS)] ALIGN_BSS(BIT(seL4_PageTableBits));

pte_t kernel_image_pt[BIT(PT_INDEX_BITS)] ALIGN_BSS(BIT(seL4_PageTableBits));
pte_t kernel_devices_pt[BIT(PT_INDEX_BITS)] ALIGN_BSS(BIT(seL4_PageTableBits));

/*
SMP_STATE_DEFINE(core_map_t, coreMap);
*/
