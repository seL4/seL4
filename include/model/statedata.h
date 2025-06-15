/*
 * Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#pragma once

#include <config.h>
#include <types.h>
#include <object/structures.h>
#include <object/tcb.h>
#include <mode/types.h>

#ifdef ENABLE_SMP_SUPPORT
#define NODE_STATE_BEGIN(_name)                 typedef struct _name {
#define NODE_STATE_END(_name)                   } _name ## _t
#define NODE_STATE_TYPE_DECLARE(_name, _state)  _name ## _t _state
#define NODE_STATE_DECLARE(_type, _state)       _type _state

#define SMP_STATE_DEFINE(_type, _state)         _type _state
#define UP_STATE_DEFINE(_type, _state)

#define SMP_COND_STATEMENT(_st)                 _st
#define SMP_TERNARY(_smp, _up)                  _smp

#define MODE_NODE_STATE_ON_CORE(_state, _core)  ksSMP[(_core)].cpu.mode._state
#define ARCH_NODE_STATE_ON_CORE(_state, _core)  ksSMP[(_core)].cpu._state
#define NODE_STATE_ON_CORE(_state, _core)       ksSMP[(_core)].system._state

#define CURRENT_CPU_INDEX() getCurrentCPUIndex()

#else

#define NODE_STATE_BEGIN(_name)
#define NODE_STATE_END(_name)
#define NODE_STATE_TYPE_DECLARE(_name, _state)
/* UP states are declared as VISIBLE so that they are accessible in assembly */
#define NODE_STATE_DECLARE(_type, _state)       extern _type _state VISIBLE

#define SMP_STATE_DEFINE(_name, _state)
#define UP_STATE_DEFINE(_type, _state)          _type _state

#define SMP_COND_STATEMENT(_st)
#define SMP_TERNARY(_smp, _up)                  _up

#define MODE_NODE_STATE_ON_CORE(_state, _core) _state
#define ARCH_NODE_STATE_ON_CORE(_state, _core) _state
#define NODE_STATE_ON_CORE(_state, _core)      _state

#define CURRENT_CPU_INDEX() SEL4_WORD_CONST(0)

#endif /* ENABLE_SMP_SUPPORT */

#define NUM_READY_QUEUES (CONFIG_NUM_DOMAINS * CONFIG_NUM_PRIORITIES)
#define L2_BITMAP_SIZE ((CONFIG_NUM_PRIORITIES + wordBits - 1) / wordBits)

NODE_STATE_BEGIN(nodeState)
NODE_STATE_DECLARE(tcb_queue_t, ksReadyQueues[NUM_READY_QUEUES]);
NODE_STATE_DECLARE(word_t, ksReadyQueuesL1Bitmap[CONFIG_NUM_DOMAINS]);
NODE_STATE_DECLARE(word_t, ksReadyQueuesL2Bitmap[CONFIG_NUM_DOMAINS][L2_BITMAP_SIZE]);
NODE_STATE_DECLARE(tcb_t, *ksCurThread);
NODE_STATE_DECLARE(tcb_t, *ksIdleThread);
NODE_STATE_DECLARE(tcb_t, *ksSchedulerAction);

#ifdef CONFIG_KERNEL_MCS
NODE_STATE_DECLARE(tcb_queue_t, ksReleaseQueue);
NODE_STATE_DECLARE(ticks_t, ksConsumed);
NODE_STATE_DECLARE(ticks_t, ksCurTime);
NODE_STATE_DECLARE(bool_t, ksReprogram);
NODE_STATE_DECLARE(sched_context_t, *ksCurSC);
NODE_STATE_DECLARE(sched_context_t, *ksIdleSC);
#endif

#ifdef CONFIG_HAVE_FPU
/* The thread using the FPU, or NULL if FPU state is invalid */
NODE_STATE_DECLARE(tcb_t *, ksCurFPUOwner);
#endif /* CONFIG_HAVE_FPU */

#ifdef CONFIG_DEBUG_BUILD
NODE_STATE_DECLARE(tcb_t *, ksDebugTCBs);
#endif /* CONFIG_DEBUG_BUILD */
#ifdef CONFIG_BENCHMARK_TRACK_UTILISATION
NODE_STATE_DECLARE(bool_t, benchmark_log_utilisation_enabled);
NODE_STATE_DECLARE(timestamp_t, benchmark_start_time);
NODE_STATE_DECLARE(timestamp_t, benchmark_end_time);
NODE_STATE_DECLARE(timestamp_t, benchmark_kernel_time);
NODE_STATE_DECLARE(timestamp_t, benchmark_kernel_number_entries);
NODE_STATE_DECLARE(timestamp_t, benchmark_kernel_number_schedules);
#endif /* CONFIG_BENCHMARK_TRACK_UTILISATION */

NODE_STATE_END(nodeState);

extern word_t ksNumCPUs;

#if defined ENABLE_SMP_SUPPORT && defined CONFIG_ARCH_ARM
#define INT_STATE_ARRAY_SIZE ((CONFIG_MAX_NUM_NODES - 1) * NUM_PPI + maxIRQ + 1)
#else
#define INT_STATE_ARRAY_SIZE (maxIRQ + 1)
#endif
extern word_t ksWorkUnitsCompleted;
extern irq_state_t intStateIRQTable[];
extern cte_t intStateIRQNode[];

extern const dschedule_t ksDomSchedule[];
extern const word_t ksDomScheduleLength;
extern word_t ksDomScheduleIdx;
extern dom_t ksCurDomain;
#ifdef CONFIG_KERNEL_MCS
extern ticks_t ksDomainTime;
#else
extern word_t ksDomainTime;
#endif

extern char ksIdleThreadTCB[CONFIG_MAX_NUM_NODES][BIT(seL4_TCBBits)];

#ifdef CONFIG_KERNEL_MCS
extern char ksIdleThreadSC[CONFIG_MAX_NUM_NODES][BIT(seL4_MinSchedContextBits)];
#endif

#ifdef CONFIG_KERNEL_LOG_BUFFER
extern paddr_t ksUserLogBuffer;
#endif /* CONFIG_KERNEL_LOG_BUFFER */

#define SchedulerAction_ResumeCurrentThread ((tcb_t*)0)
#define SchedulerAction_ChooseNewThread ((tcb_t*) 1)

#define MODE_NODE_STATE(_state)    MODE_NODE_STATE_ON_CORE(_state, getCurrentCPUIndex())
#define ARCH_NODE_STATE(_state)    ARCH_NODE_STATE_ON_CORE(_state, getCurrentCPUIndex())
#define NODE_STATE(_state)         NODE_STATE_ON_CORE(_state, getCurrentCPUIndex())
