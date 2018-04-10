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

#include <config.h>
#include <util.h>
#include <api/types.h>
#include <arch/types.h>
#include <arch/model/statedata.h>
#include <arch/object/structures.h>
#include <linker.h>
#include <plat/machine/hardware.h>

asid_pool_t *armKSASIDTable[BIT(asidHighBits)];

pgde_t armKSGlobalUserPGD[BIT(PGD_INDEX_BITS)] ALIGN_BSS(BIT(seL4_PGDBits));
pgde_t armKSGlobalKernelPGD[BIT(PGD_INDEX_BITS)] ALIGN_BSS(BIT(seL4_PGDBits));

pude_t armKSGlobalKernelPUD[BIT(PUD_INDEX_BITS)] ALIGN_BSS(BIT(seL4_PUDBits));
pde_t armKSGlobalKernelPDs[BIT(PUD_INDEX_BITS)][BIT(PD_INDEX_BITS)] ALIGN_BSS(BIT(seL4_PageDirBits));
pte_t armKSGlobalKernelPT[BIT(PT_INDEX_BITS)] ALIGN_BSS(BIT(seL4_PageTableBits));

#ifdef CONFIG_ARM_HYPERVISOR_SUPPORT
vcpu_t *armHSCurVCPU;
bool_t armHSVCPUActive;

/* The hardware VMID to virtual ASID mapping table.
 * The ARMv8 supports 8-bit VMID which is used as logical ASID
 * when the kernel runs in EL2.
 */
asid_t armKSHWASIDTable[BIT(hwASIDBits)];
hw_asid_t armKSNextASID;
#endif
