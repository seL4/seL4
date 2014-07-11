/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(GD_GPL)
 */

#ifndef __KERNEL_CDT_H
#define __KERNEL_CDT_H

#include <machine.h>
#include <object/structures.h>
#include <object/objecttype.h>

void cdtInsert(cte_t *parentSlot, cte_t *newSlot);
void cdtRemove(cte_t *slot);
cte_t *cdtFindChild(cte_t *parentSlot);
void cdtMove(cte_t *oldSlot, cte_t *newSlot);
void cdtSwap(cap_t cap1, cte_t *slot1, cap_t cap2, cte_t *slot2);
bool_t cdtIsFinal(cte_t *slot);
void cdtUpdate(cte_t *slot, cap_t newCap);
cte_t *cdtFind(cap_t hypothetical);
cte_t *cdtFindAtDepth(cap_t hypothetical, uint32_t depth);
cte_t *cdtFindWithExtra(cap_t hypothetical);
cte_t *cdtFindTypedInRange(word_t base, unsigned int size_bits);

#endif

