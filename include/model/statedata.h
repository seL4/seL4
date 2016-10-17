/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(GD_GPL)
 */

#ifndef __MODEL_STATEDATA_H
#define __MODEL_STATEDATA_H

#include <types.h>
#include <object/structures.h>
#include <object/tcb.h>
#include <arch/model/statedata.h>

NODE_STATE_BEGIN(nodeState)
NODE_STATE_TYPE_DECLARE(archNodeState, arch);
NODE_STATE_END(nodeState);

#include <arch/machine.h>
#include <arch/model/smp.h>
#include <mode/api/constants.h>

#define NUM_READY_QUEUES (CONFIG_NUM_PRIORITIES * CONFIG_NUM_CRITICALITIES)
#define L2_BITMAP_SIZE ((NUM_READY_QUEUES + seL4_WordBits - 1) / seL4_WordBits)

extern word_t ksNumCPUs VISIBLE;
extern tcb_queue_t ksReadyQueues[] VISIBLE;
extern word_t ksReadyQueuesL1Bitmap VISIBLE;
extern word_t ksReadyQueuesL2Bitmap[L2_BITMAP_SIZE] VISIBLE;
extern tcb_t *ksCurThread VISIBLE;
extern sched_context_t *ksCurSchedContext VISIBLE;
extern tcb_t *ksIdleThread VISIBLE;
extern tcb_t *ksSchedulerAction VISIBLE;
extern word_t ksWorkUnitsCompleted;
extern irq_state_t intStateIRQTable[] VISIBLE;
extern cte_t *intStateIRQNode VISIBLE;
extern time_t ksConsumed VISIBLE;
extern time_t ksCurrentTime VISIBLE;
extern bool_t ksReprogram VISIBLE;
extern tcb_t *ksReleaseHead VISIBLE;
extern crit_t ksCriticality VISIBLE;
extern tcb_queue_t ksCritQueues[] VISIBLE;
extern word_t tlbLockCount VISIBLE;

#ifdef CONFIG_BENCHMARK_USE_KERNEL_LOG_BUFFER
extern paddr_t ksUserLogBuffer;
#endif /* CONFIG_BENCHMARK_USE_KERNEL_LOG_BUFFER */

#define SchedulerAction_ResumeCurrentThread ((tcb_t*)0)
#define SchedulerAction_ChooseNewThread ((tcb_t*)~0)


#endif
