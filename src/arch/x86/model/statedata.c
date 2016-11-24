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
UP_STATE_DEFINE(interrupt_t, x86KScurInterrupt VISIBLE);

/* ==== proper read/write kernel state ==== */

/* Task State Segment (TSS), contains currently running TCB in ESP0 */
UP_STATE_DEFINE(tss_io_t, x86KStss VISIBLE);

/* Global Descriptor Table (GDT) */
UP_STATE_DEFINE(gdt_entry_t, x86KSgdt[GDT_ENTRIES]);

/* Currently active FPU state, or NULL if there is no active FPU state */
UP_STATE_DEFINE(user_fpu_state_t *, x86KSActiveFPUState);

UP_STATE_DEFINE(word_t, x86KSFPURestoresSinceSwitch);

/* The top level ASID table */
asid_pool_t* x86KSASIDTable[BIT(asidHighBits)];

/* ==== read-only kernel state (only written during bootstrapping) ==== */

/* Defines a translation of cpu ids from an index of our actual CPUs */
SMP_STATE_DEFINE(cpu_id_mapping_t, cpu_mapping);

/* Interrupt Descriptor Table (IDT) */
UP_STATE_DEFINE(idt_entry_t, x86KSidt[IDT_ENTRIES]);

/* CPU Cache Line Size */
uint32_t x86KScacheLineSizeBits;

/* A valid initial FPU state, copied to every new thread. */
user_fpu_state_t x86KSnullFpuState ALIGN(MIN_FPU_ALIGNMENT);

/* Number of IOMMUs (DMA Remapping Hardware Units) */
uint32_t x86KSnumDrhu;

/* Intel VT-d Root Entry Table */
vtd_rte_t* x86KSvtdRootTable;
uint32_t x86KSnumIOPTLevels;
uint32_t x86KSnumIODomainIDBits;
uint32_t x86KSFirstValidIODomain;

#ifdef CONFIG_VTX
UP_STATE_DEFINE(vcpu_t *, x86KSCurrentVCPU);
#endif

#ifdef CONFIG_PRINTING
uint16_t x86KSconsolePort;
#endif
#ifdef CONFIG_DEBUG_BUILD
uint16_t x86KSdebugPort;
#endif

/* State data tracking what IRQ source is related to each
 * CPU vector */
x86_irq_state_t x86KSIRQState[maxIRQ + 1];
