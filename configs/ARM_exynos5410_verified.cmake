#!/usr/bin/env -S cmake -P
#
# Copyright 2024, Proofcraft Pty Ltd
#
# SPDX-License-Identifier: GPL-2.0-only
#

include(${CMAKE_CURRENT_LIST_DIR}/include/ARM_verified_include.cmake)

set(KernelPlatform "exynos5" CACHE STRING "")
set(KernelARMPlatform "exynos5410" CACHE STRING "")
