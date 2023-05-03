#!/usr/bin/env -S cmake -P
#
# Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
#
# SPDX-License-Identifier: GPL-2.0-only
#

# If this file is executed then build the kernel.elf and kernel_all_pp.c file
include(${CMAKE_CURRENT_LIST_DIR}/../tools/helpers.cmake)
cmake_script_build_kernel()

set(KernelSel4Arch "riscv64" CACHE STRING "")
set(KernelPlatform "hifive" CACHE STRING "")
set(KernelPTLevels "3" CACHE STRING "")
set(KernelVerificationBuild ON CACHE BOOL "")
set(KernelBinaryVerificationBuild ON CACHE BOOL "")
set(KernelOptimisationCloneFunctions OFF CACHE BOOL "")
set(KernelMaxNumNodes "1" CACHE STRING "")
set(KernelOptimisation "-O2" CACHE STRING "")
set(KernelRetypeFanOutLimit "256" CACHE STRING "")
set(KernelBenchmarks "none" CACHE STRING "")
set(KernelDangerousCodeInjection OFF CACHE BOOL "")
set(KernelFastpath ON CACHE BOOL "")
set(KernelPrinting OFF CACHE BOOL "")
set(KernelNumDomains 16 CACHE STRING "")
set(KernelRootCNodeSizeBits 19 CACHE STRING "")
set(KernelMaxNumBootinfoUntypedCaps 50 CACHE STRING "")
set(KernelClzNoBuiltin ON CACHE BOOL "")
set(KernelCtzNoBuiltin ON CACHE BOOL "")
