#!/usr/bin/env -S cmake -P
#
# Copyright 2025, Proofcraft Pty Ltd
#
# SPDX-License-Identifier: GPL-2.0-only
#

include(${CMAKE_CURRENT_LIST_DIR}/include/AARCH64_verified_include.cmake)

set(KernelPlatform "zynqmp" CACHE STRING "")
set(KernelARMPlatform "ultra96v2" CACHE STRING "")
