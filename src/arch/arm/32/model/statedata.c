/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(GD_GPL)
 */

#include <util.h>
#include <api/types.h>
#include <arch/types.h>
#include <arch/model/statedata.h>
#include <arch/object/structures.h>
#include <arch/linker.h>
#include <plat/machine/hardware.h>

/* The global frame, mapped in all address spaces */
word_t armKSGlobalsFrame[BIT(ARMSmallPageBits) / sizeof(word_t)]
ALIGN_BSS(BIT(ARMSmallPageBits));

/* The top level asid mapping table */
asid_pool_t *armKSASIDTable[BIT(asidHighBits)];

/* The hardware ASID to virtual ASID mapping table */
asid_t armKSHWASIDTable[BIT(hwASIDBits)];
hw_asid_t armKSNextASID;

/* The global, privileged, physically-mapped PD */
pde_t armKSGlobalPD[BIT(PD_BITS)] ALIGN_BSS(BIT(PD_SIZE_BITS));

/* The global, privileged, page table. */
pte_t armKSGlobalPT[BIT(PT_BITS)] ALIGN_BSS(BIT(PT_SIZE_BITS));
