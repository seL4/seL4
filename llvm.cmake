#
# Copyright 2019, Data61
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

set(LLVM_TOOLCHAIN ON)
set(TOOLCHAIN_FILE "${CMAKE_CURRENT_SOURCE_DIR}/llvm.cmake")

set(CMAKE_ASM_COMPILER "clang")
set(CMAKE_ASM_COMPILER_ID Clang)
set(CMAKE_ASM_COMPILER_TARGET ${TRIPLE})

string(APPEND asm_common_flags " -Wno-unused-command-line-argument")
string(APPEND asm_common_flags " -target ${TRIPLE}")

set(CMAKE_C_COMPILER "clang")
set(C_COMPILER "clang")
set(CMAKE_C_COMPILER_ID Clang)
set(CMAKE_C_COMPILER_TARGET ${TRIPLE})

set(CMAKE_CXX_COMPILER "clang++")
set(CMAKE_CXX_COMPILER_ID Clang)
set(CMAKE_CXX_COMPILER_TARGET ${TRIPLE})

string(APPEND c_common_flags " -Qunused-arguments")
string(APPEND c_common_flags " -Wno-constant-logical-operand")
string(APPEND c_common_flags " -target ${TRIPLE}")

set(CMAKE_FIND_ROOT_PATH_MODE_PROGRAM NEVER)
set(CMAKE_FIND_ROOT_PATH_MODE_LIBRARY ONLY)
set(CMAKE_FIND_ROOT_PATH_MODE_INCLUDE ONLY)
set(CMAKE_FIND_ROOT_PATH_MODE_PACKAGE ONLY)

set(CMAKE_TRY_COMPILE_TARGET_TYPE STATIC_LIBRARY)

mark_as_advanced(FORCE CMAKE_TOOLCHAIN_FILE)
