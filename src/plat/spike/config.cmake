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

if(KernelPlatformSpike)
    config_set(KernelPlatform PLAT "spike")
    config_set(KernelPlatformFirstHartID FIRST_HART_ID 0)
    if(KernelSel4ArchRiscV32)
        list(APPEND KernelDTSList "tools/dts/spike32.dts")
    else()
        list(APPEND KernelDTSList "tools/dts/spike.dts")
    endif()
    declare_default_headers(
        TIMER_FREQUENCY 10000000llu PLIC_MAX_NUM_INT 0
        INTERRUPT_CONTROLLER arch/machine/plic.h
    )
endif()
