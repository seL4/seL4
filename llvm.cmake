#
# Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
#
# SPDX-License-Identifier: GPL-2.0-only
#

cmake_minimum_required(VERSION 3.7.2)

set(CMAKE_SYSTEM_NAME Generic)
# For a generic system this is unused, so define it to something that will be
# obvious if someone accidentally uses it
set(CMAKE_SYSTEM_PROCESSOR seL4CPU)

set(CROSS_COMPILER_PREFIX "" CACHE STRING "")

set(CMAKE_CXX_COMPILER_TARGET "" CACHE STRING "")

set(CMAKE_FIND_ROOT_PATH_MODE_PROGRAM NEVER)
set(CMAKE_FIND_ROOT_PATH_MODE_LIBRARY ONLY)
set(CMAKE_FIND_ROOT_PATH_MODE_INCLUDE ONLY)
set(CMAKE_FIND_ROOT_PATH_MODE_PACKAGE ONLY)

set(CMAKE_TRY_COMPILE_TARGET_TYPE STATIC_LIBRARY)

mark_as_advanced(FORCE CMAKE_TOOLCHAIN_FILE)

find_program(CCACHE ccache)
if(NOT ("${CCACHE}" STREQUAL CCACHE-NOTFOUND))
    set_property(GLOBAL PROPERTY RULE_LAUNCH_COMPILE ${CCACHE})
    set_property(GLOBAL PROPERTY RULE_LAUNCH_LINK ${CCACHE})
endif()
mark_as_advanced(CCACHE)
