#
# Copyright 2017, Data61
# Commonwealth Scientific and Industrial Research Organisation (CSIRO)
# ABN 41 687 119 230.
#
# This software may be distributed and modified according to the terms of
# the GNU General Public License version 2. Note that NO WARRANTY is provided.
# See "LICENSE_GPLv2.txt" for details.
#
# @TAG(DATA61_GPL)
#

cmake_minimum_required(VERSION 3.7.2)

set(CMAKE_SYSTEM_NAME Generic)
# For a generic system this is unused, so define it to something that will be
# obvious if someone accidentally uses it
set(CMAKE_SYSTEM_PROCESSOR seL4CPU)

set(CMAKE_SYSROOT "${CMAKE_BINARY_DIR}")
set(CMAKE_STAGING_PREFIX "${CMAKE_BINARY_DIR}/staging")

if("${CROSS_COMPILER_PREFIX}" STREQUAL "")
    if(AARCH32 OR ARM)
        set(CROSS_COMPILER_PREFIX "arm-linux-gnueabi-" CACHE INTERNAL "")
        if(ARM)
            message("ARM flag is deprecated, please use AARCH32")
        endif()
    elseif(AARCH32HF)
        set(CROSS_COMPILER_PREFIX "arm-linux-gnueabihf-" CACHE INTERNAL "")
    elseif(AARCH64)
        set(CROSS_COMPILER_PREFIX "aarch64-linux-gnu-" CACHE INTERNAL "")
    elseif(RISCV32)
        set(CROSS_COMPILER_PREFIX "riscv32-unknown-elf-" CACHE INTERNAL "")
    elseif(RISCV64)
        set(CROSS_COMPILER_PREFIX "riscv64-unknown-elf-" CACHE INTERNAL "")
    endif()
endif()

set(CMAKE_C_COMPILER ${CROSS_COMPILER_PREFIX}gcc)
set(CMAKE_ASM_COMPILER ${CROSS_COMPILER_PREFIX}gcc)
set(CMAKE_CXX_COMPILER ${CROSS_COMPILER_PREFIX}g++)

set(CMAKE_FIND_ROOT_PATH_MODE_PROGRAM NEVER)
set(CMAKE_FIND_ROOT_PATH_MODE_LIBRARY ONLY)
set(CMAKE_FIND_ROOT_PATH_MODE_INCLUDE ONLY)
set(CMAKE_FIND_ROOT_PATH_MODE_PACKAGE ONLY)

set(CMAKE_TRY_COMPILE_TARGET_TYPE STATIC_LIBRARY)

mark_as_advanced(FORCE CMAKE_TOOLCHAIN_FILE)
