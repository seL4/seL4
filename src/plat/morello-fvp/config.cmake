#
# Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
# Copyright (c) 2024, Capabilities Ltd <heshamalmatary@capabilitieslimited.co.uk>
#
# SPDX-License-Identifier: GPL-2.0-only
#

cmake_minimum_required(VERSION 3.7.2)

declare_platform(morello-fvp KernelPlatformMorelloFVP PLAT_MORELLO_FVP KernelSel4ArchAarch64)

if(KernelPlatformMorelloFVP)
    declare_seL4_arch(aarch64)
    set(KernelArmMorello ON)
    set(KernelArchArmV8a ON)
    set(KernelArmGicV3 ON)
    config_set(KernelARMPlatform ARM_PLAT "morello-fvp")
    list(APPEND KernelDTSList "tools/dts/morello-fvp.dts")
    list(APPEND KernelDTSList "src/plat/morello-fvp/overlay-morello-fvp.dts")
    declare_default_headers(
        TIMER_FREQUENCY 50000000
        MAX_IRQ 578
        INTERRUPT_CONTROLLER arch/machine/gic_v3.h
        TIMER drivers/timer/arm_generic.h
    )
endif()

add_sources(
    DEP "KernelPlatformMorelloFVP"
    CFILES src/arch/arm/machine/l2c_nop.c src/arch/arm/machine/gic_v3.c
)
