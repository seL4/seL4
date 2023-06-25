#!/usr/bin/env -S cmake -P
#
# Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
#
# SPDX-License-Identifier: GPL-2.0-only
#

# If this file is executed then build the kernel.elf and kernel_all_pp.c file
include(${CMAKE_CURRENT_LIST_DIR}/../tools/helpers.cmake)
cmake_script_build_kernel()

set(KernelPlatform "imx6" CACHE STRING "")
set(KernelAArch32FPUEnableContextSwitch OFF CACHE BOOL "")
set(KernelVerificationBuild ON CACHE BOOL "")
set(KernelBinaryVerificationBuild ON CACHE BOOL "")
set(KernelOptimisationCloneFunctions OFF CACHE BOOL "")
set(KernelIPCBufferLocation "threadID_register" CACHE STRING "")
set(KernelMaxNumNodes "1" CACHE STRING "")
set(KernelOptimisation "-O2" CACHE STRING "")
set(KernelRetypeFanOutLimit "256" CACHE STRING "")
set(KernelBenchmarks "none" CACHE STRING "")
set(KernelDangerousCodeInjection OFF CACHE BOOL "")
set(KernelFastpath ON CACHE BOOL "")
set(KernelPrinting OFF CACHE BOOL "")
set(KernelNumDomains 16 CACHE STRING "")
set(KernelMaxNumBootinfoUntypedCaps 230 CACHE STRING "")
set(KernelIsMCS ON CACHE BOOL "")
set(KernelStaticMaxPeriodUs "(60 * 60 * MS_IN_S * US_IN_MS)" CACHE STRING "")
