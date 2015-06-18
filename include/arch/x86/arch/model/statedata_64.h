/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(GD_GPL)
 */

#ifndef __ARCH_MODEL_STATEDATA_64_H
#define __ARCH_MODEL_STATEDATA_64_H

#include <object/structures.h>
#include <arch/types.h>

extern pml4e_t x64KSGlobalPML4[BIT(PML4_BITS)];
extern pdpte_t x64KSGlobalPDPT[BIT(PDPT_BITS)];
#ifdef CONFIG_HUGE_PAGE
extern pde_t x64KSGlobalPD[BIT(PD_BITS)];
#else
extern pde_t x64KSGlobalPDs[BIT(PDPT_BITS)][BIT(PD_BITS)];
#endif
extern pte_t x64KSGlobalPT[BIT(PT_BITS)];
extern cr3_t x64CurrentCR3;

#endif
