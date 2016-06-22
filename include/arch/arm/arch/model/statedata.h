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

#include <config.h>

#include <mode/model/statedata.h>

#ifdef CONFIG_ARM_HYPERVISOR_SUPPORT
extern pdeS1_t armHSGlobalPGD[BIT(PGD_BITS)] VISIBLE;
extern pdeS1_t armHSGlobalPD[BIT(PT_BITS)]   VISIBLE;
extern pteS1_t armHSGlobalPT[BIT(PT_BITS)]   VISIBLE;
/* Stage 2 translations have a slightly different encoding to Stage 1
 * So we need to build a User global PT for global mappings */
extern pte_t   armUSGlobalPT[BIT(PT_BITS)]   VISIBLE;
#endif /* CONFIG_ARM_HYPERVISOR_SUPPORT */

#endif /* __ARCH_MODEL_STATEDATA_H */
