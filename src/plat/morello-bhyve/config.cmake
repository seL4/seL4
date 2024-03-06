#
# Copyright 2024, Hesham Almatary (hesham.almatary@cl.cam.ac.uk)
# Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
#
# SPDX-License-Identifier: GPL-2.0-only
#

cmake_minimum_required(VERSION 3.7.2)

declare_platform(morello-bhyve KernelPlatformMorelloBhyve PLAT_MORELLO_BHYVE KernelSel4ArchAarch64)

if(KernelPlatformMorelloBhyve)

    # ARM_CPU is not currently used, but mention the name here
    # for completeness and for future Morello/CPU features that
    # may make use of this config (e.g., CHERI).
    if(NOT ARM_CPU)
        message(STATUS "ARM_CPU not set, defaulting to rainier")
        set(ARM_CPU "rainier")
    endif()

    # Currently no CHERI support exists in seL4 Morello. Future
    # support may extend the follwing config.
    if(NOT CHERI_MODE)
        message(STATUS "CHERI_MODE not set, disable it by default")
        set(CHERI_MODE "none")
    endif()

    config_set(KernelARMPlatform ARM_PLAT "morello-bhyve")
    declare_seL4_arch(aarch64)

    set(KernelArmMorello ON)
    set(KernelArchArmV8a ON)
    set(KernelArmGicV3 ON)

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
