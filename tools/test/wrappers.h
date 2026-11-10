/*
 * Copyright 2026 Indan Zupancic
 *
 * SPDX-License-Identifier: GPL-2.0-only
 *
 * Wrapper code to make seL4 SMP code compile with GenMC.
 */
#pragma once
#include <assert.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdlib.h>
#include <pthread.h>
#include <genmc.h>

/* Dummy wrappers */
#define L1_CACHE_LINE_SIZE 64
#define EXCL_RES_GRANULE_SIZE 1
#define ALIGN(n)     __attribute__((__aligned__(n)))
#define compile_assert(...)
#define arch_pause()
#define BKL_not_padded
#define BOOT_CODE
#define SchedulerAction_ResumeCurrentThread 0
#define SchedulerAction_ChooseNewThread 1
#define SCHED_ENQUEUE_CURRENT_TCB
#define fpuRelease(t)
#define CORE_IRQ_TO_IRQT(tgt, irq) (irq)
#define IRQT_TO_IDX(irq) (irq)
#define IDX_TO_IRQT(idx) (idx)
#define IRQT_TO_CORE(irqt) 0
#define IRQT_TO_IRQ(irqt) (irqt)

/* Copied from seL4 headers: */
#define NO_INLINE    __attribute__((noinline))
#define FORCE_INLINE __attribute__((always_inline))
#define PASTE(a, b) a ## b
#define _STRINGIFY(a) #a
#define STRINGIFY(a) _STRINGIFY(a)
#define UL_CONST(x) PASTE(x, ul)
#define ULL_CONST(x) PASTE(x, llu)
#define BIT(n) (UL_CONST(1) << (n))
#define MASK(n) (BIT(n) - UL_CONST(1))
#define popcountl(x) __builtin_popcountl(x)
#define clzl(x) __builtin_clzl(x)
#define cpuIndexToID(x) BIT(x)

typedef unsigned long word_t;
typedef unsigned long bool_t;
typedef unsigned long irq_t;
typedef unsigned long exception_t;

typedef enum {
    IpiRemoteCall_Stall, // Used to test a remote call to one target.
    IpiRemoteCall_Test,  // Used to test broadcast IPIs.
    IpiNumArchRemoteCall
} IpiRemoteCall_t;

/* Configuration */
#define GENMC_TEST
#define CONFIG_ENABLE_SMP_SUPPORT
#define ENABLE_SMP_SUPPORT
#define CONFIG_MAX_NUM_NODES N

/* Wrappers */
#define wordBits (sizeof(long) * 8ul)
// TODO: How to deal with, or avoid NODE_STATE_ON_CORE(). Same as usual: Make it an array instead.
#define NODE_STATE(x) x
#define irq_remote_call_ipi 0
#define irq_reschedule_ipi  1
#define arch_ipi_send_mask generic_ipi_send_mask
#define getCurrentCPUIndex() CurrentCPUIndex

__thread int CurrentCPUIndex = -1;
__thread int ksCurThread;
__thread int ksSchedulerAction;
int ksIdleThread = N;

static inline void switchToIdleThread(void)
{
    ksCurThread = ksIdleThread;
}

static inline void rescheduleRequired(void)
{
    ksSchedulerAction = SchedulerAction_ChooseNewThread;
}
