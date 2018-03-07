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

#endif /* __ARCH_MODEL_STATEDATA_H */
