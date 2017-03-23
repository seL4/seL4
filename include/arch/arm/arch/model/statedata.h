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

#include <model/statedata.h>
#include <arch/machine/debug_conf.h>
#include <mode/machine/registerset.h>

NODE_STATE_BEGIN(archNodeState)
/* TODO: add ARM-dependent fields here */
/* Bitmask of all cores should receive the reschedule IPI */
NODE_STATE_DECLARE(word_t, ipiReschedulePending);
NODE_STATE_END(archNodeState);

#ifdef ARM_BASE_CP14_SAVE_AND_RESTORE
extern user_breakpoint_state_t armKSNullBreakpointState VISIBLE;
#endif

#ifdef CONFIG_ARM_HYPERVISOR_SUPPORT
extern pdeS1_t armHSGlobalPGD[BIT(PGD_INDEX_BITS)] VISIBLE;
extern pdeS1_t armHSGlobalPD[BIT(PT_INDEX_BITS)]   VISIBLE;
extern pteS1_t armHSGlobalPT[BIT(PT_INDEX_BITS)]   VISIBLE;
#ifdef CONFIG_IPC_BUF_GLOBALS_FRAME
/* Stage 2 translations have a slightly different encoding to Stage 1
 * So we need to build a User global PT for global mappings */
extern pte_t   armUSGlobalPT[BIT(PT_INDEX_BITS)]   VISIBLE;
#endif /* CONFIG_IPC_BUF_GLOBALS_FRAME */
#endif /* CONFIG_ARM_HYPERVISOR_SUPPORT */

#endif /* __ARCH_MODEL_STATEDATA_H */
