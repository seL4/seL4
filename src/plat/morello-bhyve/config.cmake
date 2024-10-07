#
# Copyright 2024, Hesham Almatary (hesham.almatary@cl.cam.ac.uk)
# Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
#
# SPDX-License-Identifier: GPL-2.0-only
#

cmake_minimum_required(VERSION 3.7.2)

declare_platform(morello-bhyve KernelPlatformMorelloBhyve PLAT_MORELLO_BHYVE  KernelSel4ArchAarch64)

if(KernelPlatformMorelloBhyve)
    config_set(KernelARMPlatform ARM_PLAT "morello-bhyve")
    declare_seL4_arch(aarch64)

    set(KernelArmMorello ON)
    set(KernelArchArmV8a ON)
    set(KernelArmGicV3 ON)

    # CHERI config options
    set(CheriPureCapLoader OFF)

    list(APPEND KernelDTSList "tools/dts/morello-bhyve.dts")
    list(APPEND KernelDTSList "src/plat/morello-bhyve/overlay-morello-bhyve.dts")

    declare_default_headers(
        TIMER_FREQUENCY 50000000
        MAX_IRQ 578
        INTERRUPT_CONTROLLER arch/machine/gic_v3.h
        TIMER drivers/timer/arm_generic.h
    )
endif()

add_sources(
    DEP "KernelPlatformMorelloBhyve"
    CFILES src/arch/arm/machine/l2c_nop.c src/arch/arm/machine/gic_v3.c
)
