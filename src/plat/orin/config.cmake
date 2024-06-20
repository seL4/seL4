#
# Copyright 2023, NIO
#
# SPDX-License-Identifier: GPL-2.0-only
#

cmake_minimum_required(VERSION 3.7.2)

declare_platform(orin KernelPlatformOrin PLAT_ORIN KernelSel4ArchAarch64)

if(KernelPlatformOrin)
    config_set(KernelARMPlatform ARM_PLAT orin)
    declare_seL4_arch(aarch64)
    set(KernelArmCortexA78AE ON)
    set(KernelArchArmV8a ON)

    set(KernelArmGicV3 ON)
    set(KernelArmMach "nvidia" CACHE INTERNAL "")

    list(APPEND KernelDTSList "tools/dts/orin.dts" "src/plat/orin/overlay-orin.dts")

    declare_default_headers(
        TIMER_FREQUENCY 31250000
        MAX_IRQ 1019
        TIMER drivers/timer/arm_generic.h
        INTERRUPT_CONTROLLER arch/machine/gic_v3.h
        NUM_PPI 32
        CLK_MAGIC 1llu
        CLK_SHIFT 3u
        KERNEL_WCET 10u
    )
endif()

add_sources(
    DEP "KernelPlatformOrin"
    CFILES src/arch/arm/machine/gic_v3.c src/arch/arm/machine/l2c_nop.c
)
