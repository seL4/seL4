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
#include <api/debug.h>
#include <types.h>
#include <plat/machine.h>
#include <model/statedata.h>
#include <object/structures.h>
#include <object/tcb.h>

/* Pointer to the head of the scheduler queue for each priority */
tcb_queue_t ksReadyQueues[NUM_READY_QUEUES];
word_t ksReadyQueuesL1Bitmap;
word_t ksReadyQueuesL2Bitmap[L2_BITMAP_SIZE];
compile_assert(ksReadyQueuesL1BitmapBigEnough, (L2_BITMAP_SIZE - 1) <= wordBits)

/* Current thread TCB pointer */
tcb_t *ksCurThread;

/* Idle thread TCB pointer */
tcb_t *ksIdleThread;

/* current scheduling context pointer */
sched_context_t *ksCurSchedContext;

/* Values of 0 and ~0 encode ResumeCurrentThread and ChooseNewThread
 * respectively; other values encode SwitchToThread and must be valid
 * tcb pointers */
tcb_t *ksSchedulerAction;

/* Units of work we have completed since the last time we checked for
 * pending interrupts */
word_t ksWorkUnitsCompleted;

/* CNode containing interrupt handler endpoints */
irq_state_t intStateIRQTable[maxIRQ + 1];
cte_t *intStateIRQNode;

/* the amount of time passed since the kernel time was last updated */
ticks_t ksConsumed;
/* the current kernel time (recorded on kernel entry) */
ticks_t ksCurrentTime;
/* whether we need to reprogram the timer before exiting the kernel */
bool_t ksReprogram;
/* head of the queue of threads waiting for their budget to be replenished */
tcb_t *ksReleaseHead;
/* current criticality level of the kernel */
crit_t ksCriticality;
/* list of active threads at each criticality */
tcb_queue_t ksCritQueues[CONFIG_NUM_CRITICALITIES];

/* Only used by lockTLBEntry */
word_t tlbLockCount = 0;

#ifdef DEBUG
debug_entry_t ksKernelEntry;
#endif /* DEBUG */
