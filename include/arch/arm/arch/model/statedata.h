/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(GD_GPL)
 */

#ifndef __ARCH_MODEL_STATEDATA_H
#define __ARCH_MODEL_STATEDATA_H

#include <types.h>
#include <arch/types.h>
#include <util.h>
#include <object/structures.h>

extern word_t armKSGlobalsFrame[BIT(ARMSmallPageBits) / sizeof(word_t)] VISIBLE;
extern asid_pool_t *armKSASIDTable[BIT(asidHighBits)] VISIBLE;
extern asid_t armKSHWASIDTable[BIT(hwASIDBits)] VISIBLE;
extern hw_asid_t armKSNextASID VISIBLE;
#ifndef ARM_HYP
extern pde_t armKSGlobalPD[BIT(PD_BITS)] VISIBLE;
extern pte_t armKSGlobalPT[BIT(PT_BITS)] VISIBLE;
#else
extern pdeS1_t armHSGlobalPGD[BIT(PGD_BITS)] VISIBLE;
extern pdeS1_t armHSGlobalPD[BIT(PT_BITS)]   VISIBLE;
extern pteS1_t armHSGlobalPT[BIT(PT_BITS)]   VISIBLE;
/* Stage 2 translations have a slightly different encoding to Stage 1
 * So we need to build a User global PT for global mappings */
extern pte_t   armUSGlobalPT[BIT(PT_BITS)]   VISIBLE;
#endif /* ARM_HYP */

#endif /* __ARCH_MODEL_STATEDATA_H */
