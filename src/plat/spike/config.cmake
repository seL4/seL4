#
# Copyright 2018, Data61
# Commonwealth Scientific and Industrial Research Organisation (CSIRO)
# ABN 41 687 119 230.
#
# Copyright 2018, DornerWorks
#
# This software may be distributed and modified according to the terms of
# the GNU General Public License version 2. Note that NO WARRANTY is provided.
# See "LICENSE_GPLv2.txt" for details.
#
# @TAG(DATA61_DORNERWORKS_GPL)
#

cmake_minimum_required(VERSION 3.7.2)

declare_platform(spike KernelPlatformSpike PLAT_SPIKE KernelArchRiscV)

if(KernelPlatformSpike)
    if("${KernelSel4Arch}" STREQUAL riscv32)
        declare_seL4_arch(riscv32)
    elseif("${KernelSel4Arch}" STREQUAL riscv64)
        declare_seL4_arch(riscv64)
    else()
        fallback_declare_seL4_arch_default(riscv64)
    endif()
    config_set(KernelPlatformFirstHartID FIRST_HART_ID 0)
    if(KernelSel4ArchRiscV32)
        list(APPEND KernelDTSList "tools/dts/spike32.dts")
    else()
        list(APPEND KernelDTSList "tools/dts/spike.dts")
    endif()
    list(APPEND KernelDTSList "src/plat/spike/overlay-spike.dts")
    declare_default_headers(
        TIMER_FREQUENCY 10000000llu PLIC_MAX_NUM_INT 0
        INTERRUPT_CONTROLLER arch/machine/plic.h
    )
else()
    unset(KernelPlatformFirstHartID CACHE)
endif()
