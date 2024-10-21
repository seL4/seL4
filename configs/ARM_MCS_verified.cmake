#!/usr/bin/env -S cmake -P
#
# Copyright 2024, Proofcraft Pty Ltd
#
# SPDX-License-Identifier: GPL-2.0-only
#

include(${CMAKE_CURRENT_LIST_DIR}/include/ARM_verified_include.cmake)

set(KernelPlatform "imx6" CACHE STRING "")
set(KernelIsMCS ON CACHE BOOL "")
set(KernelStaticMaxPeriodUs "(60 * 60 * MS_IN_S * US_IN_MS)" CACHE STRING "")
