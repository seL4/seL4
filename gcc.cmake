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

# When this file is passed to configure_file in cmake, these variables get set to
# the kernel platform configuration.
set(sel4_arch @KernelSel4Arch@)
set(arch @KernelArch@)
set(mode @KernelWordSize@)

if("${CROSS_COMPILER_PREFIX}" STREQUAL "")
    if(("${arch}" STREQUAL "arm") OR ("${arch}" STREQUAL "x86") OR ("${arch}" STREQUAL "riscv"))
        if(${sel4_arch} STREQUAL "aarch32" OR ${sel4_arch} STREQUAL "arm_hyp")
            set(CROSS_COMPILER_PREFIX "arm-linux-gnueabi-" CACHE INTERNAL "")
        elseif(${sel4_arch} STREQUAL "aarch64")
            set(CROSS_COMPILER_PREFIX "aarch64-linux-gnu-" CACHE INTERNAL "")
        elseif(${arch} STREQUAL "riscv")
            set(CROSS_COMPILER_PREFIX "riscv64-unknown-linux-gnu-" CACHE INTERNAL "")
        endif()
    else()
        # For backwards compatibility reasons we allow this file to work without templating.
        # If initialised with -DCMAKE_TOOLCHAIN_FILE="$SCRIPT_PATH/gcc.cmake" this script
        # understood the following arguments: ARM, AARCH32, AARCH32HF, AARCH64, RISCV32, RISCV64, APPLE
        if(AARCH32 OR ARM)
            set(CROSS_COMPILER_PREFIX "arm-linux-gnueabi-" CACHE INTERNAL "")
            if(ARM)
                message("ARM flag is deprecated, please use AARCH32")
            endif()
        elseif(AARCH64)
            set(CROSS_COMPILER_PREFIX "aarch64-linux-gnu-" CACHE INTERNAL "")
        elseif(RISCV32 OR RISCV64)
            set(CROSS_COMPILER_PREFIX "riscv64-unknown-linux-gnu-" CACHE INTERNAL "")
        endif()
    endif()
    if(AARCH32HF)
        set(CROSS_COMPILER_PREFIX "arm-linux-gnueabihf-" CACHE INTERNAL "")
    endif()

    if("${CROSS_COMPILER_PREFIX}" STREQUAL "")
        # If we haven't set a target above we assume x86_64/ia32 target
        if(APPLE)
            # APPLE is a CMake variable that evaluates to True on a Mac OSX system
            set(CROSS_COMPILER_PREFIX "x86_64-unknown-linux-gnu-" CACHE INTERNAL "")
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
if ($ENV{GCC_COLORS})
    set(coloured_output ON)
else()
    set(coloured_output OFF)
endif()
option(FORCE_COLORED_OUTPUT "Always produce ANSI-colored output." ${coloured_output})
mark_as_advanced(FORCE_COLORED_OUTPUT)
if (${FORCE_COLORED_OUTPUT})
    include_guard(GLOBAL)
    add_compile_options(-fdiagnostics-color=always)
endif ()
