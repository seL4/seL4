/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(GD_GPL)
 */

#ifndef __CONFIG_H
#define __CONFIG_H

/* Compile-time configuration parameters. Might be set by the build system. */

#ifdef HAVE_AUTOCONF
#include <autoconf.h>
#endif

/* size of the initial thread's root CNode (2^x slots, x >= 4) */
#ifndef CONFIG_ROOT_CNODE_SIZE_BITS
#define CONFIG_ROOT_CNODE_SIZE_BITS 12
#endif

/* number of timer ticks until a thread is preempted  */
#ifndef CONFIG_TIME_SLICE
#define CONFIG_TIME_SLICE 5
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
/* CONSTRAINT: (5 * CONFIG_MAX_NUM_BOOTINFO_UNTYPED_CAPS) <= 4036 */
#ifndef CONFIG_MAX_NUM_BOOTINFO_UNTYPED_CAPS
#define CONFIG_MAX_NUM_BOOTINFO_UNTYPED_CAPS 800
#endif

/* length of a timer tick in ms  */
#ifndef CONFIG_TIMER_TICK_MS
#define CONFIG_TIMER_TICK_MS 2
#endif

/* maximum number of different tracepoints which can be placed in the kernel */
#ifndef CONFIG_MAX_NUM_TRACE_POINTS
#define CONFIG_MAX_NUM_TRACE_POINTS 0
#endif

/* Configuration parameters below are for IA-32 only. */

/* maximum number of nodes supported (if 1, a uniprocessor version is compiled) */
#ifndef CONFIG_MAX_NUM_NODES
#define CONFIG_MAX_NUM_NODES 8 /* must be between 1 and 256 */
#endif

/* maximum number of memory regions to report in bootinfo */
#ifndef CONFIG_MAX_MEM_REGIONS
#define CONFIG_MAX_MEM_REGIONS 10
#endif

/* maximum number of IOMMU RMRR entries we can record while ACPI parsing */

#ifndef CONFIG_MAX_RMRR_ENTRIES
#define CONFIG_MAX_RMRR_ENTRIES 32
#endif

#endif /* __CONFIG_H */
