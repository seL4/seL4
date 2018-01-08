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

#include <arch/model/statedata.h>

/* The privileged kernel mapping PD & PT */
pml4e_t x64KSKernelPML4[BIT(PML4_INDEX_BITS)] ALIGN(BIT(seL4_PML4Bits)) VISIBLE;
pdpte_t x64KSKernelPDPT[BIT(PDPT_INDEX_BITS)] ALIGN(BIT(seL4_PDPTBits));
#ifdef CONFIG_HUGE_PAGE
pde_t x64KSKernelPD[BIT(PD_INDEX_BITS)] ALIGN(BIT(seL4_PageDirBits));
#else
pde_t x64KSKernelPDs[BIT(PDPT_INDEX_BITS)][BIT(PD_INDEX_BITS)] ALIGN(BIT(seL4_PageDirBits));
#endif
pte_t x64KSKernelPT[BIT(PT_INDEX_BITS)] ALIGN(BIT(seL4_PageTableBits));

#ifdef CONFIG_KERNEL_SKIM_WINDOW
pml4e_t x64KSSKIMPML4[BIT(PML4_INDEX_BITS)] ALIGN(BIT(seL4_PML4Bits));
pdpte_t x64KSSKIMPDPT[BIT(PDPT_INDEX_BITS)] ALIGN(BIT(seL4_PDPTBits));
pde_t x64KSSKIMPD[BIT(PD_INDEX_BITS)] ALIGN(BIT(seL4_PageDirBits));
#endif

#ifdef CONFIG_KERNEL_SKIM_WINDOW
UP_STATE_DEFINE(word_t, x64KSCurrentUserCR3);
#else
UP_STATE_DEFINE(cr3_t, x64KSCurrentCR3);
#endif

word_t x64KSIRQStack[CONFIG_MAX_NUM_NODES][IRQ_STACK_SIZE + 2] ALIGN(64) VISIBLE SKIM_BSS;
