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

extern tcb_queue_t ksReadyQueues[] VISIBLE;
extern word_t ksReadyQueuesL1Bitmap VISIBLE;
extern word_t ksReadyQueuesL2Bitmap[(CONFIG_NUM_PRIORITIES / wordBits) + 1] VISIBLE;
extern tcb_t *ksCurThread VISIBLE;
extern tcb_t *ksIdleThread VISIBLE;
extern tcb_t *ksSchedulerAction VISIBLE;
extern word_t ksWorkUnitsCompleted;
extern irq_state_t intStateIRQTable[] VISIBLE;
extern cte_t *intStateIRQNode VISIBLE;
extern time_t ksConsumed VISIBLE;
extern time_t ksCurrentTime VISIBLE;
extern bool_t ksReprogram VISIBLE;

#define SchedulerAction_ResumeCurrentThread ((tcb_t*)0)
#define SchedulerAction_ChooseNewThread ((tcb_t*)~0)

#define NUM_READY_QUEUES (CONFIG_NUM_PRIORITIES)

#endif
