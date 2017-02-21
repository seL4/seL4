/*
 * Copyright 2016, Data61
 * Commonwealth Scientific and Industrial Research Organisation (CSIRO)
 * ABN 41 687 119 230.
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(DATA61_GPL)
 */

#ifndef __ARCH_MODE_MODEL_STATEDATA_H_
#define __ARCH_MODE_MODEL_STATEDATA_H_

#include <object/structures.h>
#include <arch/types.h>
#include <model/statedata.h>

extern pml4e_t x64KSGlobalPML4[BIT(PML4_INDEX_BITS)];
extern pdpte_t x64KSGlobalPDPT[BIT(PDPT_INDEX_BITS)];
#ifdef CONFIG_HUGE_PAGE
extern pde_t x64KSGlobalPD[BIT(PD_INDEX_BITS)];
#else
extern pde_t x64KSGlobalPDs[BIT(PDPT_INDEX_BITS)][BIT(PD_INDEX_BITS)];
#endif
extern pte_t x64KSGlobalPT[BIT(PT_INDEX_BITS)];

NODE_STATE_BEGIN(modeNodeState)
NODE_STATE_DECLARE(cr3_t, x64KSCurrentCR3);
/* hardware interrupt handlers push up to 6 words onto the stack. The order of the
 words is Error, RIP, CS, FLAGS, RSP, SS */
#define IRQ_STACK_SIZE 6
NODE_STATE_DECLARE(word_t, x64KSIRQStack[IRQ_STACK_SIZE] ALIGN(16));
NODE_STATE_END(modeNodeState);

#endif /* __ARCH_MODE_MODEL_STATEDATA_H_ */
