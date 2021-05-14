/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * SPDX-License-Identifier: GPL-2.0-only
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

UP_STATE_DEFINE(interrupt_t, x86KSPendingInterrupt);

/* ==== proper read/write kernel state ==== */

x86_arch_global_state_t x86KSGlobalState[CONFIG_MAX_NUM_NODES] ALIGN(L1_CACHE_LINE_SIZE) SKIM_BSS;

/* The top level ASID table */
asid_pool_t *x86KSASIDTable[BIT(asidHighBits)];

/* Current user value of the fs/gs base */
UP_STATE_DEFINE(word_t, x86KSCurrentFSBase);
UP_STATE_DEFINE(word_t, x86KSCurrentGSBase);

UP_STATE_DEFINE(word_t, x86KSGPExceptReturnTo);

/* ==== read-only kernel state (only written during bootstrapping) ==== */

/* Defines a translation of cpu ids from an index of our actual CPUs */
SMP_STATE_DEFINE(cpu_id_mapping_t, cpu_mapping);

/* CPU Cache Line Size */
uint32_t x86KScacheLineSizeBits;

/* A valid initial FPU state, copied to every new thread. */
user_fpu_state_t x86KSnullFpuState ALIGN(MIN_FPU_ALIGNMENT);

/* Number of IOMMUs (DMA Remapping Hardware Units) */
uint32_t x86KSnumDrhu;

#ifdef CONFIG_IOMMU
/* Intel VT-d Root Entry Table */
vtd_rte_t *x86KSvtdRootTable;
uint32_t x86KSnumIOPTLevels;
uint32_t x86KSnumIODomainIDBits;
uint32_t x86KSFirstValidIODomain;
#endif

#ifdef CONFIG_VTX
UP_STATE_DEFINE(vcpu_t *, x86KSCurrentVCPU);
#endif

#ifdef CONFIG_PRINTING
uint16_t x86KSconsolePort;
#endif
#if defined(CONFIG_PRINTING) || defined(CONFIG_DEBUG_BUILD)
uint16_t x86KSdebugPort;
#endif

/* State data tracking what IRQ source is related to each
 * CPU vector */
x86_irq_state_t x86KSIRQState[maxIRQ + 1];

word_t x86KSAllocatedIOPorts[NUM_IO_PORTS / CONFIG_WORD_SIZE];
#ifdef CONFIG_KERNEL_MCS
uint32_t x86KStscMhz;
uint32_t x86KSapicRatio;
#endif
