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
#include <util.h>
#include <api/types.h>
#include <arch/types.h>
#include <arch/model/statedata.h>
#include <arch/object/structures.h>

/* ==== read/write kernel state not preserved across kernel entries ==== */

/* Interrupt currently being handled */
interrupt_t ia32KScurInterrupt VISIBLE;

/* ==== proper read/write kernel state ==== */

/* Task State Segment (TSS), contains currently running TCB in ESP0 */
tss_t ia32KStss VISIBLE;

/* Global Descriptor Table (GDT) */
gdt_entry_t ia32KSgdt[GDT_ENTRIES];

/*
 * Current thread whose state is installed in the FPU, or NULL if
 * the FPU is currently invalid.
 */
tcb_t *ia32KSfpuOwner VISIBLE;

/* ==== read-only kernel state (only written during bootstrapping) ==== */

/* The privileged kernel mapping PD & PT */
pdpte_t* ia32KSkernelPDPT;
pde_t* ia32KSkernelPD;
pte_t* ia32KSkernelPT;

/* CPU Cache Line Size */
uint32_t ia32KScacheLineSizeBits;

/* Interrupt Descriptor Table (IDT) */
idt_entry_t ia32KSidt[IDT_ENTRIES];

/* A valid initial FPU state, copied to every new thread. */
user_fpu_state_t ia32KSnullFpuState ALIGN(MIN_FPU_ALIGNMENT);

/* Current active page directory. This is really just a shadow of CR3 */
paddr_t ia32KSCurrentPD VISIBLE;

#ifdef CONFIG_IOMMU
/* Number of IOMMUs (DMA Remapping Hardware Units) */
uint32_t ia32KSnumDrhu;

/* Intel VT-d Root Entry Table */
vtd_rte_t* ia32KSvtdRootTable;
uint32_t ia32KSnumIOPTLevels;
uint32_t ia32KSnumIODomainIDBits;
int ia32KSFirstValidIODomain;
#endif

#if defined DEBUG || defined RELEASE_PRINTF
uint16_t ia32KSconsolePort;
uint16_t ia32KSdebugPort;
#endif

node_id_t ia32KSNodeID;
uint32_t ia32KSNumNodes;
cpu_id_t* ia32KSCPUList;
