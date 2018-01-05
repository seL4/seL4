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

#ifndef __MODEL_STATEDATA_H_
#define __MODEL_STATEDATA_H_

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

#define CURRENT_CPU_INDEX() 0

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

#ifdef CONFIG_HAVE_FPU
/* Current state installed in the FPU, or NULL if the FPU is currently invalid */
NODE_STATE_DECLARE(user_fpu_state_t *, ksActiveFPUState);
/* Number of times we have restored a user context with an active FPU without switching it */
NODE_STATE_DECLARE(word_t, ksFPURestoresSinceSwitch);
#endif /* CONFIG_HAVE_FPU */
#ifdef CONFIG_DEBUG_BUILD
NODE_STATE_DECLARE(tcb_t *, ksDebugTCBs);
#endif /* CONFIG_DEBUG_BUILD */

NODE_STATE_END(nodeState);

extern word_t ksNumCPUs;

extern word_t ksWorkUnitsCompleted;
extern irq_state_t intStateIRQTable[];
extern cte_t *intStateIRQNode;
extern const dschedule_t ksDomSchedule[];
extern const word_t ksDomScheduleLength;
extern word_t ksDomScheduleIdx;
extern dom_t ksCurDomain;
extern word_t ksDomainTime;
extern word_t tlbLockCount VISIBLE;

#ifdef CONFIG_BENCHMARK_USE_KERNEL_LOG_BUFFER
extern paddr_t ksUserLogBuffer;
#endif /* CONFIG_BENCHMARK_USE_KERNEL_LOG_BUFFER */

#define SchedulerAction_ResumeCurrentThread ((tcb_t*)0)
#define SchedulerAction_ChooseNewThread ((tcb_t*) 1)

#define MODE_NODE_STATE(_state)    MODE_NODE_STATE_ON_CORE(_state, getCurrentCPUIndex())
#define ARCH_NODE_STATE(_state)    ARCH_NODE_STATE_ON_CORE(_state, getCurrentCPUIndex())
#define NODE_STATE(_state)         NODE_STATE_ON_CORE(_state, getCurrentCPUIndex())

#endif /* __MODEL_STATEDATA_H_ */
