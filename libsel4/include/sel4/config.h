/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#pragma once

/* Compile-time configuration parameters. Might be set by the build system. */

#include <autoconf.h>

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
