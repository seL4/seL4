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
interrupt_t x86KScurInterrupt VISIBLE;

/* ==== proper read/write kernel state ==== */

/* Task State Segment (TSS), contains currently running TCB in ESP0 */
tss_io_t x86KStss VISIBLE;

/* Global Descriptor Table (GDT) */
gdt_entry_t x86KSgdt[GDT_ENTRIES];

/* The top level ASID table */
asid_pool_t* x86KSASIDTable[BIT(asidHighBits)];

/*
 * Current thread whose state is installed in the FPU, or NULL if
 * the FPU is currently invalid.
 */
tcb_t *x86KSfpuOwner VISIBLE;

/* ==== read-only kernel state (only written during bootstrapping) ==== */

/* CPU Cache Line Size */
uint32_t x86KScacheLineSizeBits;

/* Interrupt Descriptor Table (IDT) */
idt_entry_t x86KSidt[IDT_ENTRIES];

/* A valid initial FPU state, copied to every new thread. */
user_fpu_state_t x86KSnullFpuState ALIGN(MIN_FPU_ALIGNMENT);

/* frequency of the tsc */
uint32_t x86KStscMhz;

/* Number of IOMMUs (DMA Remapping Hardware Units) */
uint32_t x86KSnumDrhu;

/* Intel VT-d Root Entry Table */
vtd_rte_t* x86KSvtdRootTable;
uint32_t x86KSnumIOPTLevels;
uint32_t x86KSnumIODomainIDBits;
uint32_t x86KSFirstValidIODomain;

#ifdef CONFIG_PRINTING
uint16_t x86KSconsolePort;
uint16_t x86KSdebugPort;
#endif

/* State data tracking what IRQ source is related to each
 * CPU vector */
x86_irq_state_t x86KSIRQState[maxIRQ + 1];
