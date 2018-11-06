#
# Copyright 2017, Data61
# Commonwealth Scientific and Industrial Research Organisation (CSIRO)
# ABN 41 687 119 230.
# Copyright 2018, Mokshasoft AB (mokshasoft.com)
#
# This software may be distributed and modified according to the terms of
# the GNU General Public License version 2. Note that NO WARRANTY is provided.
# See "LICENSE_GPLv2.txt" for details.
#
# @TAG(DATA61_GPL)
#

cmake_minimum_required(VERSION 3.7.2)

macro(set_cross_compiler_prefix)
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
        else()
            # If we haven't set a target above we assume x86_64/ia32 target
            if(APPLE)
                # APPLE is a CMake variable that evaluates to True on a Mac OSX system
                set(CROSS_COMPILER_PREFIX "x86_64-unknown-linux-gnu-" CACHE INTERNAL "")
            endif()
        endif()
    endif()
endmacro()
