/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(GD_GPL)
 */

#include <config.h>
#include <util.h>
#include <api/types.h>
#include <arch/types.h>
#include <arch/model/statedata.h>
#include <arch/object/structures.h>
#include <arch/machine/debug_conf.h>
#include <linker.h>
#include <plat/machine/hardware.h>

#ifdef CONFIG_IPC_BUF_GLOBALS_FRAME
/* The global frame, mapped in all address spaces */
word_t armKSGlobalsFrame[BIT(ARMSmallPageBits) / sizeof(word_t)]
ALIGN_BSS(BIT(ARMSmallPageBits));
#endif /* CONFIG_IPC_BUF_GLOBALS_FRAME */

/* The top level asid mapping table */
asid_pool_t *armKSASIDTable[BIT(asidHighBits)];

/* The hardware ASID to virtual ASID mapping table */
asid_t armKSHWASIDTable[BIT(hwASIDBits)];
hw_asid_t armKSNextASID;

#ifndef CONFIG_ARM_HYPERVISOR_SUPPORT
/* The global, privileged, physically-mapped PD */
pde_t armKSGlobalPD[BIT(PD_INDEX_BITS)] ALIGN_BSS(BIT(seL4_PageDirBits));

/* The global, privileged, page table. */
pte_t armKSGlobalPT[BIT(PT_INDEX_BITS)] ALIGN_BSS(BIT(seL4_PageTableBits));

#ifdef CONFIG_BENCHMARK_USE_KERNEL_LOG_BUFFER
pte_t armKSGlobalLogPT[BIT(PT_INDEX_BITS)] ALIGN_BSS(BIT(seL4_PageTableBits));
#endif /* CONFIG_BENCHMARK_USE_KERNEL_LOG_BUFFER */

#else
/* The global, hypervisor, level 1 page table */
pdeS1_t  armHSGlobalPGD[BIT(PGD_INDEX_BITS)] ALIGN_BSS(BIT(PGD_SIZE_BITS));
/* The global, hypervisor, level 2 page table */
pdeS1_t  armHSGlobalPD[BIT(PT_INDEX_BITS)]   ALIGN_BSS(BIT(seL4_PageTableBits));
/* The global, hypervisor, level 3 page table */
pteS1_t  armHSGlobalPT[BIT(PT_INDEX_BITS)]   ALIGN_BSS(BIT(seL4_PageTableBits));
/* Global user space mappings, which are empty as there is no hared kernel region */
pde_t armUSGlobalPD[BIT(PD_INDEX_BITS)] ALIGN_BSS(BIT(seL4_PageDirBits));;
#ifdef CONFIG_IPC_BUF_GLOBALS_FRAME
/* User space global mappings */
pte_t  armUSGlobalPT[BIT(PT_INDEX_BITS)]   ALIGN_BSS(BIT(seL4_PageTableBits));
#endif /* CONFIG_IPC_BUF_GLOBALS_FRAME */
/* Current VCPU */
vcpu_t *armHSCurVCPU;
/* Whether the current loaded VCPU is enabled in the hardware or not */
bool_t armHSVCPUActive;
#endif /* CONFIG_ARM_HYPERVISOR_SUPPORT */

#ifdef ARM_BASE_CP14_SAVE_AND_RESTORE
/* Null state for the Debug coprocessor's break/watchpoint registers */
user_breakpoint_state_t armKSNullBreakpointState;
#endif
