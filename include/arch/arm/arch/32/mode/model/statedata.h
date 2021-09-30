/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#pragma once

#include <config.h>
#include <types.h>
#include <arch/types.h>
#include <util.h>
#include <object/structures.h>

extern asid_pool_t *armKSASIDTable[BIT(asidHighBits)] VISIBLE;
extern asid_t armKSHWASIDTable[BIT(hwASIDBits)] VISIBLE;
extern hw_asid_t armKSNextASID VISIBLE;

#ifndef CONFIG_ARM_HYPERVISOR_SUPPORT
extern pde_t armKSGlobalPD[BIT(PD_INDEX_BITS)] VISIBLE;
extern pte_t armKSGlobalPT[BIT(PT_INDEX_BITS)] VISIBLE;

#ifdef CONFIG_KERNEL_LOG_BUFFER
extern pte_t armKSGlobalLogPT[BIT(PT_INDEX_BITS)] VISIBLE;
#endif /* CONFIG_KERNEL_LOG_BUFFER */

#else
extern pdeS1_t armHSGlobalPGD[BIT(PGD_INDEX_BITS)] VISIBLE;
extern pdeS1_t armHSGlobalPD[BIT(PT_INDEX_BITS)]   VISIBLE;
extern pteS1_t armHSGlobalPT[BIT(PT_INDEX_BITS)]   VISIBLE;
extern pde_t armUSGlobalPD[BIT(PD_INDEX_BITS)] VISIBLE;
/* Stage 2 translations have a slightly different encoding to Stage 1
 * So we need to build a User global PT for global mappings */
extern pte_t   armUSGlobalPT[BIT(PT_INDEX_BITS)]   VISIBLE;
#endif /* CONFIG_ARM_HYPERVISOR_SUPPORT */

