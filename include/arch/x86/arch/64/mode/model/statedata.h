/*
 * Copyright 2017, Data61
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
NODE_STATE_END(modeNodeState);

/* hardware interrupt handlers push up to 6 words onto the stack. The order of the
 words is Error, RIP, CS, FLAGS, RSP, SS. It also needs to be 16byte aligned for
 the hardware to push correctly. For better SMP performance we add 2 words to the
 array (such that each array is 64 bytes) and align to 64 bytes as well (which is
 the typical L1 cache line size). Note that we do not align to the L1_CACHE_LINE_SZ
 macro as that *could* be configured to be less than 16, which would be incorrect
 for us here */
#define IRQ_STACK_SIZE 6
extern word_t x64KSIRQStack[CONFIG_MAX_NUM_NODES][IRQ_STACK_SIZE + 2] ALIGN(64) VISIBLE;

#endif /* __ARCH_MODE_MODEL_STATEDATA_H_ */
