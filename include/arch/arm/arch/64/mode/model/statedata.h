/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#pragma once

#include <config.h>
#include <types.h>
#include <arch/types.h>
#include <util.h>
#include <object/structures.h>

#ifdef CONFIG_ARM_SMMU
#include <arch/object/smmu.h>
#endif

#ifdef CONFIG_ALLOW_SMC_CALLS
#include <arch/object/smc.h>
#endif

/* The top level asid mapping table */
extern asid_pool_t *armKSASIDTable[BIT(asidHighBits)] VISIBLE;

/* This is the temporary userspace page table in kernel. It is required before running
 * user thread to avoid speculative page table walking with the wrong page table. */
extern vspace_root_t armKSGlobalUserVSpace[BIT(seL4_VSpaceIndexBits)] VISIBLE;
extern pte_t armKSGlobalKernelPGD[BIT(PT_INDEX_BITS)] VISIBLE;

extern pte_t armKSGlobalKernelPUD[BIT(PT_INDEX_BITS)] VISIBLE;
extern pte_t armKSGlobalKernelPDs[BIT(PT_INDEX_BITS)][BIT(PT_INDEX_BITS)] VISIBLE;
extern pte_t armKSGlobalKernelPT[BIT(PT_INDEX_BITS)] VISIBLE;

#ifdef CONFIG_ARM_HYPERVISOR_SUPPORT

extern asid_t armKSHWASIDTable[BIT(hwASIDBits)] VISIBLE;
extern hw_asid_t armKSNextASID VISIBLE;
#endif

#ifdef CONFIG_KERNEL_LOG_BUFFER
pde_t *armKSGlobalLogPDE;
#endif


#ifdef CONFIG_ARM_SMMU
extern bool_t smmuStateSIDTable[SMMU_MAX_SID];
extern cte_t smmuStateSIDNode[BIT(SMMU_SID_CNODE_SLOT_BITS)];
extern bool_t smmuStateCBTable[SMMU_MAX_CB];
extern cte_t smmuStateCBNode[BIT(SMMU_CB_CNODE_SLOT_BITS)];
#endif
