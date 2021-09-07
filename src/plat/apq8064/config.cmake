#
# Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
#
# SPDX-License-Identifier: GPL-2.0-only
#

cmake_minimum_required(VERSION 3.7.2)

declare_platform(apq8064 KernelPlatformAPQ8064 PLAT_APQ8064 KernelSel4ArchAarch32)

if(KernelPlatformAPQ8064)
    declare_seL4_arch(aarch32)

    # MCS is not supported on apq8064. It requires a timer driver that
    # implements the tickless programming requirements.
    set(KernelPlatformSupportsMCS OFF)

    set(KernelArmCortexA15 ON)
    set(KernelArchArmV7a ON)
    set(KernelArchArmV7ve ON)
    config_set(KernelARMPlatform ARM_PLAT apq8064)
    list(APPEND KernelDTSList "tools/dts/apq8064.dts")
    list(APPEND KernelDTSList "src/plat/apq8064/overlay-apq8064.dts")

    declare_default_headers(
        TIMER_FREQUENCY 7000000
        MAX_IRQ 283
        NUM_PPI 32
        TIMER drivers/timer/arm_generic.h
        INTERRUPT_CONTROLLER arch/machine/gic_v2.h
    )
endif()

add_sources(
    DEP "KernelPlatformAPQ8064"
    CFILES src/arch/arm/machine/gic_v2.c src/arch/arm/machine/l2c_nop.c
)
