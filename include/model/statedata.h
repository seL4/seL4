/*
 * Copyright 2016, Data61
 * Commonwealth Scientific and Industrial Research Organisation (CSIRO)
 * ABN 41 687 119 230.
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(D61_GPL)
 */

#pragma once

#include <config.h>
#include <types.h>
#include <object/structures.h>
#include <object/tcb.h>
#include <mode/types.h>
#include <model/smp.h>
#include <arch/machine.h>

#define NUM_READY_QUEUES (CONFIG_NUM_DOMAINS * CONFIG_NUM_PRIORITIES)

NODE_STATE_BEGIN(nodeState)
NODE_STATE_DECLARE(tcb_queue_t, ksReadyQueues[NUM_READY_QUEUES]);
NODE_STATE_DECLARE(word_t, ksReadyQueuesL1Bitmap[CONFIG_NUM_DOMAINS]);
NODE_STATE_DECLARE(word_t, ksReadyQueuesL2Bitmap[CONFIG_NUM_DOMAINS][(CONFIG_NUM_PRIORITIES / wordBits) + 1]);
NODE_STATE_DECLARE(tcb_t, *ksCurThread);
NODE_STATE_DECLARE(tcb_t, *ksIdleThread);
NODE_STATE_DECLARE(tcb_t, *ksSchedulerAction);
NODE_STATE_END(nodeState);

extern word_t ksNumCPUs VISIBLE;

extern word_t ksWorkUnitsCompleted;
extern irq_state_t intStateIRQTable[] VISIBLE;
extern cte_t *intStateIRQNode VISIBLE;
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
#define SchedulerAction_ChooseNewThread ((tcb_t*)~0)
