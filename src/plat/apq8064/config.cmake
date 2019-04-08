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

if(KernelPlatformAPQ8064)
    set(KernelArmCortexA15 ON)
    set(KernelArchArmV7a ON)
    set(KernelArchArmV7ve ON)
    config_set(KernelPlatform PLAT "apq8064")
    list(APPEND KernelDTSList "tools/dts/apq8064.dts")
    list(APPEND KernelDTSList "src/plat/apq8064/overlay-apq8064.dts")

    declare_default_headers(
        TIMER_FREQUENCY 7000000llu
        MAX_IRQ 283
        TIMER arch/machine/generic_timer.h
        INTERRUPT_CONTROLLER arch/machine/gic_pl390.h
    )
endif()

add_sources(
    DEP "KernelPlatformAPQ8064"
    CFILES src/arch/arm/machine/gic_pl390.c src/arch/arm/machine/l2c_nop.c
)
