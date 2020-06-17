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

/* The top level asid mapping table */
extern asid_pool_t *armKSASIDTable[BIT(asidHighBits)] VISIBLE;

/* This is the temporary userspace page table in kernel. It is required before running
 * user thread to avoid speculative page table walking with the wrong page table. */
extern vspace_root_t armKSGlobalUserVSpace[BIT(seL4_VSpaceIndexBits)] VISIBLE;
extern pgde_t armKSGlobalKernelPGD[BIT(PGD_INDEX_BITS)] VISIBLE;

extern pude_t armKSGlobalKernelPUD[BIT(PUD_INDEX_BITS)] VISIBLE;
extern pde_t armKSGlobalKernelPDs[BIT(PUD_INDEX_BITS)][BIT(PD_INDEX_BITS)] VISIBLE;
extern pte_t armKSGlobalKernelPT[BIT(PT_INDEX_BITS)] VISIBLE;

#ifdef CONFIG_ARM_HYPERVISOR_SUPPORT

extern asid_t armKSHWASIDTable[BIT(hwASIDBits)] VISIBLE;
extern hw_asid_t armKSNextASID VISIBLE;
#endif

#ifdef CONFIG_BENCHMARK_USE_KERNEL_LOG_BUFFER
pde_t *armKSGlobalLogPDE;
#endif

