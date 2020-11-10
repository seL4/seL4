/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#pragma once

/* Compile-time configuration parameters. Might be set by the build system. */

#ifdef HAVE_AUTOCONF
#include <autoconf.h>
#endif

/* size of the initial thread's root CNode (2^x slots, x >= 4) */
#ifndef CONFIG_ROOT_CNODE_SIZE_BITS
#define CONFIG_ROOT_CNODE_SIZE_BITS 12
#endif

/* number of timer ticks until a thread is preempted  */
#ifndef CONFIG_KERNEL_MCS
#ifndef CONFIG_TIME_SLICE
#define CONFIG_TIME_SLICE 5
#endif
#endif

#ifdef CONFIG_KERNEL_MCS
#ifndef CONFIG_BOOT_THREAD_TIME_SLICE
#define CONFIG_BOOT_THREAD_TIME_SLICE 5
#endif

#ifndef CONFIG_KERNEL_WCET_SCALE
#define CONFIG_KERNEL_WCET_SCALE 1
#endif
#endif

/* the number of scheduler domains */
#ifndef CONFIG_NUM_DOMAINS
#define CONFIG_NUM_DOMAINS 16
#endif

/* number of priorities per domain */
#ifndef CONFIG_NUM_PRIORITIES
#define CONFIG_NUM_PRIORITIES 256
#endif

/* maximum number of caps that can be created in one retype invocation */
#ifndef CONFIG_RETYPE_FAN_OUT_LIMIT
#define CONFIG_RETYPE_FAN_OUT_LIMIT 256
#endif

/* chunk size for memory clears during retype, in bits. */
#ifndef CONFIG_RESET_CHUNK_BITS
#define CONFIG_RESET_CHUNK_BITS 8
#endif

/* maximum number of iterations until we preempt a delete/revoke invocation */
#ifndef CONFIG_MAX_NUM_WORK_UNITS_PER_PREEMPTION
#define CONFIG_MAX_NUM_WORK_UNITS_PER_PREEMPTION 100
#endif

/* address range to flush per preemption work unit */
#ifndef CONFIG_FLUSH_WORK_UNIT
#define CONFIG_FLUSH_WORK_UNIT 64
#endif

/* maximum number of untyped caps in bootinfo */
/* WARNING: must match value in libsel4! */
/* CONSTRAINT: (16 * CONFIG_MAX_NUM_BOOTINFO_DEVICE_REGIONS) + (5 * CONFIG_MAX_NUM_BOOTINFO_UNTYPED_CAPS) <= 4036 */
#ifndef CONFIG_MAX_NUM_BOOTINFO_UNTYPED_CAPS
#define CONFIG_MAX_NUM_BOOTINFO_UNTYPED_CAPS 166
#endif

#ifndef CONFIG_KERNEL_MCS
/* length of a timer tick in ms  */
#ifndef CONFIG_TIMER_TICK_MS
#define CONFIG_TIMER_TICK_MS 2
#endif
#endif

/* maximum number of different tracepoints which can be placed in the kernel */
#ifndef CONFIG_MAX_NUM_TRACE_POINTS
#define CONFIG_MAX_NUM_TRACE_POINTS 0
#endif

/* maximum number of IOMMU RMRR entries we can record while ACPI parsing */
#ifndef CONFIG_MAX_RMRR_ENTRIES
#define CONFIG_MAX_RMRR_ENTRIES 32
#endif

/* maximum number of IOAPIC supported */
#ifndef CONFIG_MAX_NUM_IOAPIC
#define CONFIG_MAX_NUM_IOAPIC  1
#endif

/* Alias CONFIG_MAX_NUM_NODES > 1 to ENABLE_SMP_SUPPORT */
#if CONFIG_MAX_NUM_NODES > 1
#define ENABLE_SMP_SUPPORT
#endif

#ifdef CONFIG_ARM_HYPERVISOR_SUPPORT
#ifdef CONFIG_ARM_PA_SIZE_BITS_40
#define AARCH64_VSPACE_S2_START_L1
#endif
#endif

/* Configurations requring the kernel log buffer */
#if defined CONFIG_BENCHMARK_TRACK_KERNEL_ENTRIES || \
    defined CONFIG_BENCHMARK_TRACEPOINTS
#define CONFIG_KERNEL_LOG_BUFFER
#endif
