/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(GD_GPL)
 */

#include <arch/model/statedata.h>

/* The privileged kernel mapping PD & PT */
pml4e_t x64KSGlobalPML4[BIT(PML4_BITS)] ALIGN(BIT(PML4_SIZE_BITS));
pdpte_t x64KSGlobalPDPT[BIT(PDPT_BITS)] ALIGN(BIT(PDPT_SIZE_BITS));
#ifdef CONFIG_HUGE_PAGE
pde_t x64KSGlobalPD[BIT(PD_BITS)] ALIGN(BIT(PD_SIZE_BITS));
#else
pde_t x64KSGlobalPDs[BIT(PDPT_BITS)][BIT(PD_BITS)] ALIGN(BIT(PD_SIZE_BITS));
#endif
pte_t x64KSGlobalPT[BIT(PT_BITS)] ALIGN(BIT(PT_SIZE_BITS));

cr3_t x64CurrentCR3;
