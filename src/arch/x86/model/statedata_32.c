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
pdpte_t ia32KSGlobalPDPT[BIT(PDPT_BITS)] ALIGN(BIT(PDPT_SIZE_BITS));
pde_t ia32KSGlobalPD[BIT(PD_BITS + PDPT_BITS)] ALIGN(BIT(PD_SIZE_BITS));
pte_t ia32KSGlobalPT[BIT(PT_BITS)] ALIGN(BIT(PT_SIZE_BITS));

/* Current active page directory. This is really just a shadow of CR3 */
paddr_t ia32KSCurrentPD VISIBLE;
