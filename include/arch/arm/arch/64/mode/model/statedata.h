/*
 * Copyright 2017, Data61
 * Commonwealth Scientific and Industrial Research Organisation (CSIRO)
 * ABN 41 687 119 230.
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(DATA61_GPL)
 */

#ifndef __ARCH_MODEL_STATEDATA_64_H
#define __ARCH_MODEL_STATEDATA_64_H

#include <config.h>
#include <types.h>
#include <arch/types.h>
#include <util.h>
#include <object/structures.h>

/* The top level asid mapping table */
extern asid_pool_t *armKSASIDTable[BIT(asidHighBits)] VISIBLE;

/* This is the temporary userspace page table in kernel. It is required before running
 * user thread to avoid speculative page table walking with the wrong page table. */
extern pgde_t armKSGlobalUserPGD[BIT(PGD_INDEX_BITS)] VISIBLE;
extern pgde_t armKSGlobalKernelPGD[BIT(PGD_INDEX_BITS)] VISIBLE;

extern pude_t armKSGlobalKernelPUD[BIT(PUD_INDEX_BITS)] VISIBLE;
extern pde_t armKSGlobalKernelPDs[BIT(PUD_INDEX_BITS)][BIT(PD_INDEX_BITS)] VISIBLE;
extern pte_t armKSGlobalKernelPT[BIT(PT_INDEX_BITS)] VISIBLE;

#ifdef CONFIG_ARM_HYPERVISOR_SUPPORT
extern vcpu_t *armHSCurVCPU;
extern bool_t armHSVCPUActive;

extern asid_t armKSHWASIDTable[BIT(hwASIDBits)] VISIBLE;
extern hw_asid_t armKSNextASID VISIBLE;
#endif
#endif /* __ARCH_MODEL_STATEDATA_64_H */
