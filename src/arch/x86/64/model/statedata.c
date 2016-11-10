/*
 * Copyright 2016, Data61
 * Commonwealth Scientific and Industrial Research Organisation (CSIRO)
 * ABN 41 687 119 230.
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(D61_GPL)
 */

#include <arch/model/statedata.h>

/* The privileged kernel mapping PD & PT */
pml4e_t x64KSGlobalPML4[BIT(PML4_INDEX_BITS)] ALIGN(BIT(seL4_PML4Bits));
pdpte_t x64KSGlobalPDPT[BIT(PDPT_INDEX_BITS)] ALIGN(BIT(seL4_PDPTBits));
#ifdef CONFIG_HUGE_PAGE
pde_t x64KSGlobalPD[BIT(PD_INDEX_BITS)] ALIGN(BIT(seL4_PageDirBits));
#else
pde_t x64KSGlobalPDs[BIT(PDPT_INDEX_BITS)][BIT(PD_INDEX_BITS)] ALIGN(BIT(seL4_PageDirBits));
#endif
pte_t x64KSGlobalPT[BIT(PT_INDEX_BITS)] ALIGN(BIT(seL4_PageTableBits));

UP_STATE_DEFINE(cr3_t, x64KSCurrentCR3);
UP_STATE_DEFINE(word_t, x64KSIRQStack[IRQ_STACK_SIZE] ALIGN(16));
