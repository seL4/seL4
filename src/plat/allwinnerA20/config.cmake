#
# Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
#
# SPDX-License-Identifier: GPL-2.0-only
#

cmake_minimum_required(VERSION 3.7.2)

declare_platform(allwinnerA20 KernelPlatformAllwinnerA20 PLAT_ALLWINNERA20 KernelSel4ArchAarch32)

if(KernelPlatformAllwinnerA20)
    declare_seL4_arch(aarch32)
    set(KernelArmCortexA7 ON)
    set(KernelArchArmV7a ON)
    config_set(KernelARMPlatform ARM_PLAT allwinnerA20)

    # MCS is not supported on allwinnerA20. It requires a timer driver that
    # implements the tickless programming requirements.
    set(KernelPlatformSupportsMCS OFF)

    list(APPEND KernelDTSList "tools/dts/allwinnerA20.dts")
    list(APPEND KernelDTSList "src/plat/allwinnerA20/overlay-allwinnerA20.dts")

    declare_default_headers(
        TIMER_FREQUENCY 24000000
        MAX_IRQ 122
        NUM_PPI 32
        TIMER drivers/timer/arm_generic.h
        INTERRUPT_CONTROLLER arch/machine/gic_v2.h
    )
endif()

add_sources(
    DEP "KernelPlatformAllwinnerA20"
    CFILES src/plat/allwinnerA20/machine/l2cache.c src/arch/arm/machine/gic_v2.c
)
