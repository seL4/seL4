/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#pragma once

#include <config.h>
#include <arch/object/vcpu.h>
#include <model/statedata.h>
#include <arch/machine/debug_conf.h>
#include <mode/machine/registerset.h>

NODE_STATE_BEGIN(archNodeState)
/* TODO: add ARM-dependent fields here */
/* Bitmask of all cores should receive the reschedule IPI */
NODE_STATE_DECLARE(word_t, ipiReschedulePending);
#ifdef CONFIG_ARM_HYPERVISOR_SUPPORT
NODE_STATE_DECLARE(vcpu_t, *armHSCurVCPU);
NODE_STATE_DECLARE(bool_t, armHSVCPUActive);
#if defined(CONFIG_ARCH_AARCH32) && defined(CONFIG_HAVE_FPU)
NODE_STATE_DECLARE(bool_t, armHSFPUEnabled);
#endif
#endif
#if defined(CONFIG_BENCHMARK_TRACK_UTILISATION) && defined(KERNEL_PMU_IRQ)
NODE_STATE_DECLARE(uint64_t, ccnt_num_overflows);
#endif /* defined(CONFIG_BENCHMARK_TRACK_UTILISATION) && defined(KERNEL_PMU_IRQ) */
NODE_STATE_END(archNodeState);

#ifdef ARM_BASE_CP14_SAVE_AND_RESTORE
extern user_breakpoint_state_t armKSNullBreakpointState VISIBLE;
#endif
