#
# Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
#
# SPDX-License-Identifier: GPL-2.0-only
#

cmake_minimum_required(VERSION 3.7.2)

declare_platform(quartz64 KernelPlatformQuartz64 PLAT_QUARTZ64 KernelSel4ArchAarch64)

if(KernelPlatformQuartz64)

    declare_seL4_arch(aarch64)
    set(KernelArmCortexA55 ON)
    set(KernelArchArmV8a ON)
    set(KernelArmGicV3 ON)
    config_set(KernelARMPlatform ARM_PLAT "quartz64")
    list(APPEND KernelDTSList "tools/dts/quartz64.dts")
    list(APPEND KernelDTSList "src/plat/quartz64/overlay-quartz64.dts")
    declare_default_headers(
        TIMER_FREQUENCY 24000000
        MAX_IRQ 231
        NUM_PPI 32
        KERNEL_WCET 10u
        TIMER drivers/timer/arm_generic.h
        INTERRUPT_CONTROLLER arch/machine/gic_v3.h
    )
endif()

add_sources(
    DEP "KernelPlatformQuartz64"
    CFILES src/arch/arm/machine/gic_v3.c src/arch/arm/machine/l2c_nop.c
)
