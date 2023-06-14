/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * SPDX-License-Identifier: GPL-2.0-only
 */

#include <config.h>
#include <api/debug.h>
#include <types.h>
#include <plat/machine.h>
#include <model/statedata.h>
#include <model/smp.h>
#include <object/structures.h>
#include <object/tcb.h>
#include <benchmark/benchmark_track.h>

/* Collective cpu states, including both pre core architecture dependant and independent data */
SMP_STATE_DEFINE(smpStatedata_t, ksSMP[CONFIG_MAX_NUM_NODES] ALIGN(L1_CACHE_LINE_SIZE));

/* Global count of how many cpus there are */
word_t ksNumCPUs;

/* Pointer to the head of the scheduler queue for each priority */
UP_STATE_DEFINE(tcb_queue_t, ksReadyQueues[NUM_READY_QUEUES]);
UP_STATE_DEFINE(word_t, ksReadyQueuesL1Bitmap[CONFIG_NUM_DOMAINS]);
UP_STATE_DEFINE(word_t, ksReadyQueuesL2Bitmap[CONFIG_NUM_DOMAINS][L2_BITMAP_SIZE]);
compile_assert(ksReadyQueuesL1BitmapBigEnough, (L2_BITMAP_SIZE - 1) <= wordBits)
#ifdef CONFIG_KERNEL_MCS
/* Head of the queue of threads waiting for their budget to be replenished */
UP_STATE_DEFINE(tcb_queue_t, ksReleaseQueue);
#endif

/* Current thread TCB pointer */
UP_STATE_DEFINE(tcb_t *, ksCurThread);

/* Idle thread TCB pointer */
UP_STATE_DEFINE(tcb_t *, ksIdleThread);

/* Values of 0 and ~0 encode ResumeCurrentThread and ChooseNewThread
 * respectively; other values encode SwitchToThread and must be valid
 * tcb pointers */
UP_STATE_DEFINE(tcb_t *, ksSchedulerAction);

#ifdef CONFIG_HAVE_FPU
/* Currently active FPU state, or NULL if there is no active FPU state */
UP_STATE_DEFINE(user_fpu_state_t *, ksActiveFPUState);

UP_STATE_DEFINE(word_t, ksFPURestoresSinceSwitch);
#endif /* CONFIG_HAVE_FPU */
#ifdef CONFIG_KERNEL_MCS
/* the amount of time passed since the kernel time was last updated */
UP_STATE_DEFINE(ticks_t, ksConsumed);
/* whether we need to reprogram the timer before exiting the kernel */
UP_STATE_DEFINE(bool_t, ksReprogram);
/* the current kernel time (recorded on kernel entry) */
UP_STATE_DEFINE(ticks_t, ksCurTime);
/* current scheduling context pointer */
UP_STATE_DEFINE(sched_context_t *, ksCurSC);
UP_STATE_DEFINE(sched_context_t *, ksIdleSC);
#endif

#ifdef CONFIG_DEBUG_BUILD
UP_STATE_DEFINE(tcb_t *, ksDebugTCBs);
#endif /* CONFIG_DEBUG_BUILD */
#ifdef CONFIG_BENCHMARK_TRACK_UTILISATION
UP_STATE_DEFINE(bool_t, benchmark_log_utilisation_enabled);
UP_STATE_DEFINE(timestamp_t, benchmark_start_time);
UP_STATE_DEFINE(timestamp_t, benchmark_end_time);
UP_STATE_DEFINE(timestamp_t, benchmark_kernel_time);
UP_STATE_DEFINE(timestamp_t, benchmark_kernel_number_entries);
UP_STATE_DEFINE(timestamp_t, benchmark_kernel_number_schedules);
#endif /* CONFIG_BENCHMARK_TRACK_UTILISATION */

/* Units of work we have completed since the last time we checked for
 * pending interrupts */
word_t ksWorkUnitsCompleted;

irq_state_t intStateIRQTable[INT_STATE_ARRAY_SIZE];
/* CNode containing interrupt handler endpoints - like all seL4 objects, this CNode needs to be
 * of a size that is a power of 2 and aligned to its size. */
cte_t intStateIRQNode[BIT(IRQ_CNODE_SLOT_BITS)] ALIGN(BIT(IRQ_CNODE_SLOT_BITS + seL4_SlotBits));
compile_assert(irqCNodeSize, sizeof(intStateIRQNode) >= ((INT_STATE_ARRAY_SIZE) *sizeof(cte_t)));

/* Currently active domain */
dom_t ksCurDomain;

/* Domain timeslice remaining */
#ifdef CONFIG_KERNEL_MCS
ticks_t ksDomainTime;
#else
word_t ksDomainTime;
#endif

/* An index into ksDomSchedule for active domain and length. */
word_t ksDomScheduleIdx;

/* Only used by lockTLBEntry */
word_t tlbLockCount = 0;

/* Idle thread. */
SECTION("._idle_thread") char ksIdleThreadTCB[CONFIG_MAX_NUM_NODES][BIT(seL4_TCBBits)] ALIGN(BIT(TCB_SIZE_BITS));

#ifdef CONFIG_KERNEL_MCS
/* Idle thread Schedcontexts */
char ksIdleThreadSC[CONFIG_MAX_NUM_NODES][BIT(seL4_MinSchedContextBits)] ALIGN(BIT(seL4_MinSchedContextBits));
#endif

#if (defined CONFIG_DEBUG_BUILD || defined CONFIG_BENCHMARK_TRACK_KERNEL_ENTRIES)
kernel_entry_t ksKernelEntry;
#endif /* DEBUG */

#ifdef CONFIG_KERNEL_LOG_BUFFER
paddr_t ksUserLogBuffer;
#endif /* CONFIG_KERNEL_LOG_BUFFER */
