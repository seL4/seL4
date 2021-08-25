/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#pragma once

#include <config.h>
#include <object/structures.h>
#include <arch/types.h>
#include <model/statedata.h>

extern pml4e_t x64KSKernelPML4[BIT(PML4_INDEX_BITS)] VISIBLE;
extern pdpte_t x64KSKernelPDPT[BIT(PDPT_INDEX_BITS)];
#ifdef CONFIG_HUGE_PAGE
extern pde_t x64KSKernelPD[BIT(PD_INDEX_BITS)];
#else
extern pde_t x64KSKernelPDs[BIT(PDPT_INDEX_BITS)][BIT(PD_INDEX_BITS)];
#endif
extern pte_t x64KSKernelPT[BIT(PT_INDEX_BITS)];

#ifdef CONFIG_KERNEL_SKIM_WINDOW
extern pml4e_t x64KSSKIMPML4[BIT(PML4_INDEX_BITS)] ALIGN(BIT(seL4_PML4Bits));
extern pdpte_t x64KSSKIMPDPT[BIT(PDPT_INDEX_BITS)] ALIGN(BIT(seL4_PDPTBits));
/* we only need a single PD regardless of huge pages or not as the skim window
   just has a few 2M entries out of the 1gb region of the kernel image */
extern pde_t x64KSSKIMPD[BIT(PD_INDEX_BITS)] ALIGN(BIT(seL4_PageDirBits));
#endif

NODE_STATE_BEGIN(modeNodeState)
#ifdef CONFIG_KERNEL_SKIM_WINDOW
/* we declare this as a word_t and not a cr3_t as we cache both state and potentially
 * command information (state being pml4 base and pcid) and command being whether or not
 * to flush translation. the formal cr3_t type only talks about the state */
NODE_STATE_DECLARE(word_t, x64KSCurrentUserCR3);
#else
NODE_STATE_DECLARE(cr3_t, x64KSCurrentCR3);
#endif
NODE_STATE_END(modeNodeState);

/* hardware interrupt handlers push up to 6 words onto the stack. The order of the
 words is Error, RIP, CS, FLAGS, RSP, SS. It also needs to be 16byte aligned for
 the hardware to push correctly. For better SMP performance we add 2 words to the
 array (such that each array is 64 bytes) and align to 64 bytes as well (which is
 the typical L1 cache line size). Note that we do not align to the L1_CACHE_LINE_SZ
 macro as that *could* be configured to be less than 16, which would be incorrect
 for us here */
#define IRQ_STACK_SIZE 6
extern word_t x64KSIRQStack[CONFIG_MAX_NUM_NODES][IRQ_STACK_SIZE + 2] ALIGN(64) VISIBLE SKIM_BSS;

