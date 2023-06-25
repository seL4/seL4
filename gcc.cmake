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

set(CMAKE_SYSROOT "${CMAKE_BINARY_DIR}")

# When this file is passed to configure_file in cmake, these variables get set to
# the kernel platform configuration.
set(sel4_arch @KernelSel4Arch@)
set(arch @KernelArch@)
set(mode @KernelWordSize@)
set(cross_prefix @cross_prefix@)

# If this file is used without templating, then cross_prefix will
# have an invalid value and should only be assigned to CROSS_COMPILER_PREFIX
# if it has been set to something different.
# We need to build the test string dynamically otherwise the templating would
# overwrite it.
set(cross_prefix_test @cross_prefix)
string(APPEND cross_prefix_test @)
if((NOT "${cross_prefix}" STREQUAL "${cross_prefix_test}") AND (NOT "${cross_prefix}" STREQUAL ""))
    set(CROSS_COMPILER_PREFIX ${cross_prefix})
endif()

# This function hunts for an extant `gcc` with one of the candidate prefixes
# specified in `ARGN`, allowing us to try different target triple prefixes for
# cross-compilers built in various ways.
function(FindPrefixedGCC out_var)
    set("${out_var}" "PrefixedGCC-NOTFOUND")
    foreach(prefix ${ARGN})
        set("test_var" "_GCC_${prefix}")
        find_program("${test_var}" "${prefix}gcc")
        if(${test_var})
            message(STATUS "Found GCC with prefix ${prefix}")
            set("${out_var}" "${prefix}")
            break()
        endif()
    endforeach()
    if(${out_var})
        set("${out_var}" "${${out_var}}" PARENT_SCOPE)
    else()
        message(FATAL_ERROR "Unable to find valid cross-compiling GCC")
    endif()
endfunction(FindPrefixedGCC)

if("${CROSS_COMPILER_PREFIX}" STREQUAL "")
    if(("${arch}" STREQUAL "arm") OR ("${arch}" STREQUAL "x86") OR ("${arch}" STREQUAL "riscv"))
        if(${sel4_arch} STREQUAL "aarch32" OR ${sel4_arch} STREQUAL "arm_hyp")
            FindPrefixedGCC(
                CROSS_COMPILER_PREFIX
                "arm-linux-gnueabi-"
                "arm-linux-gnu-"
                "arm-none-eabi-"
            )
        elseif(${sel4_arch} STREQUAL "aarch64")
            FindPrefixedGCC(
                CROSS_COMPILER_PREFIX
                "aarch64-linux-gnu-"
                "aarch64-unknown-linux-gnu-"
                "aarch64-none-linux-gnu-"
                "aarch64-none-elf-"
            )
        elseif(${arch} STREQUAL "riscv")
            FindPrefixedGCC(
                CROSS_COMPILER_PREFIX
                "riscv64-unknown-linux-gnu-"
                "riscv64-unknown-elf-"
                "riscv64-elf-"
            )
        endif()
    else()
        # For backwards compatibility reasons we allow this file to work without templating.
        # If initialised with -DCMAKE_TOOLCHAIN_FILE="$SCRIPT_PATH/gcc.cmake" this script
        # understood the following arguments: ARM, AARCH32, AARCH32HF, AARCH64, RISCV32, RISCV64, APPLE
        if(AARCH32 OR ARM)
            FindPrefixedGCC(
                CROSS_COMPILER_PREFIX
                "arm-linux-gnueabi-"
                "arm-linux-gnu-"
                "arm-none-eabi-"
            )
            if(ARM)
                message("ARM flag is deprecated, please use AARCH32")
            endif()
        elseif(AARCH64)
            FindPrefixedGCC(
                CROSS_COMPILER_PREFIX
                "aarch64-linux-gnu-"
                "aarch64-unknown-linux-gnu-"
                "aarch64-none-linux-gnu-"
                "aarch64-none-elf-"
            )
        elseif(RISCV32 OR RISCV64)
            FindPrefixedGCC(
                CROSS_COMPILER_PREFIX
                "riscv64-unknown-linux-gnu-"
                "riscv64-unknown-elf-"
                "riscv64-elf-"
            )
        endif()
    endif()
    if(AARCH32HF)
        FindPrefixedGCC(
            CROSS_COMPILER_PREFIX
            "arm-linux-gnueabihf-"
            "arm-linux-gnu-" # Later checks should confirm this has `hardfp`
        )
    endif()

    if("${CROSS_COMPILER_PREFIX}" STREQUAL "")
        # If we haven't set a target above we assume x86_64/ia32 target, and hence have to
        # find an appropriate x86 compatible toolchain. If we're on an AArch64 host, GCC will
        # not support x86. If we're on macOS (detected via CMAKE_HOST_APPLE), we need to find
        # the appropriate x86 compiler regardless of host architecture. This is as `gcc`
        # usually actually uses clang on macOS, not GCC.
        if("${CMAKE_HOST_SYSTEM_PROCESSOR}" STREQUAL "aarch64" OR CMAKE_HOST_APPLE)
            if("${sel4_arch}" STREQUAL "ia32")
                FindPrefixedGCC(CROSS_COMPILER_PREFIX "i686-linux-gnu-" "i686-unknown-linux-gnu-")
            else()
                FindPrefixedGCC(
                    CROSS_COMPILER_PREFIX "x86_64-linux-gnu-" "x86_64-unknown-linux-gnu-"
                )
            endif()
        endif()
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

# Invoke compiler via ccache. This has no effect if ccache cannot be found.
# Projects can override this effect by resetting the RULE_LAUNCH_COMPILE and
# RULE_LAUNCH_LINK properties.
find_program(CCACHE ccache)
if(NOT ("${CCACHE}" STREQUAL CCACHE-NOTFOUND))
    set_property(GLOBAL PROPERTY RULE_LAUNCH_COMPILE ${CCACHE})
    set_property(GLOBAL PROPERTY RULE_LAUNCH_LINK ${CCACHE})
endif()
mark_as_advanced(CCACHE)

# GCC color options:
# Ninja and ccache cause gcc to not emit colored output when -fdiagnostics-color=auto.
# We upgrade this to -fdiagnostics-color=always if FORCE_COLORED_OUTPUT is set
# We default FORCE_COLORED_OUTPUT=ON if GCC_COLORS is set in the environment
# otherwise FORCE_COLORED_OUTPUT is left off.
if($ENV{GCC_COLORS})
    set(coloured_output ON)
else()
    set(coloured_output OFF)
endif()
option(FORCE_COLORED_OUTPUT "Always produce ANSI-colored output." ${coloured_output})
mark_as_advanced(FORCE_COLORED_OUTPUT)
if(${FORCE_COLORED_OUTPUT})
    include_guard(GLOBAL)
    add_compile_options(-fdiagnostics-color=always)
endif()
