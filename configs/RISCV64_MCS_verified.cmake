#!/usr/bin/env -S cmake -P
#
# Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
#
# SPDX-License-Identifier: GPL-2.0-only
#

include(${CMAKE_CURRENT_LIST_DIR}/include/RISCV64_verified_include.cmake)

set(KernelPlatform "hifive" CACHE STRING "")
set(KernelIsMCS ON CACHE BOOL "")
set(KernelStaticMaxPeriodUs "(60 * 60 * MS_IN_S * US_IN_MS)" CACHE STRING "")
