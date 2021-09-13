#
# Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
#
# SPDX-License-Identifier: GPL-2.0-only
#

cmake_minimum_required(VERSION 3.7.2)

declare_platform(fvp KernelPlatformFVP PLAT_FVP KernelSel4ArchAarch64)

if(KernelPlatformFVP)
    declare_seL4_arch(aarch64)
    set(KernelArmCortexA57 ON)
    set(KernelArchArmV8a ON)
    set(KernelArmGicV3 ON)
    config_set(KernelARMPlatform ARM_PLAT "fvp")
    list(APPEND KernelDTSList "tools/dts/fvp.dts")
    list(APPEND KernelDTSList "src/plat/fvp/overlay-fvp.dts")
    declare_default_headers(
        TIMER_FREQUENCY 100000000
        MAX_IRQ 207
        INTERRUPT_CONTROLLER arch/machine/gic_v3.h
        TIMER drivers/timer/arm_generic.h
    )
endif()

add_sources(
    DEP "KernelPlatformFVP"
    CFILES src/arch/arm/machine/l2c_nop.c src/arch/arm/machine/gic_v3.c
)
