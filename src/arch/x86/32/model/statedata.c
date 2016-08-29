/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(GD_GPL)
 */

#include <config.h>
#include <arch/model/statedata.h>

/* The privileged kernel mapping PD & PT */
pdpte_t ia32KSGlobalPDPT[BIT(PDPT_INDEX_BITS)] ALIGN(BIT(seL4_PDPTBits));
pde_t ia32KSGlobalPD[BIT(PD_INDEX_BITS + PDPT_INDEX_BITS)] ALIGN(BIT(seL4_PageDirBits));
pte_t ia32KSGlobalPT[BIT(PT_INDEX_BITS)] ALIGN(BIT(seL4_PageTableBits));

#ifdef CONFIG_BENCHMARK_USE_KERNEL_LOG_BUFFER
pte_t ia32KSGlobalLogPT[BIT(PT_INDEX_BITS)] ALIGN(BIT(seL4_PageTableBits));
#endif /* CONFIG_BENCHMARK_USE_KERNEL_LOG_BUFFER */

/* Current active page directory. This is really just a shadow of CR3 */
paddr_t ia32KSCurrentPD VISIBLE;
