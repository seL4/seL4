#
# Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
#
# SPDX-License-Identifier: GPL-2.0-only
#

cmake_minimum_required(VERSION 3.7.2)

declare_platform(rockpro64 KernelPlatformRockpro64 PLAT_ROCKPRO64 KernelSel4ArchAarch64)

if(KernelPlatformRockpro64)

    declare_seL4_arch(aarch64)
    set(KernelArmCortexA53 ON)
    set(KernelArchArmV8a ON)
    set(KernelArmGicV3 ON)
    config_set(KernelARMPlatform ARM_PLAT "rockpro64")
    list(APPEND KernelDTSList "tools/dts/rockpro64.dts")
    list(APPEND KernelDTSList "src/plat/rockpro64/overlay-rockpro64.dts")
    declare_default_headers(
        TIMER_FREQUENCY 24000000
        MAX_IRQ 181
        NUM_PPI 32
        KERNEL_WCET 10u
        TIMER drivers/timer/arm_generic.h
        INTERRUPT_CONTROLLER arch/machine/gic_v3.h
    )
endif()

add_sources(
    DEP "KernelPlatformRockpro64"
    CFILES src/arch/arm/machine/gic_v3.c src/arch/arm/machine/l2c_nop.c
)
