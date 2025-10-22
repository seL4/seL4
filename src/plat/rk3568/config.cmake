#
# Copyright 2025, UNSW
#
# SPDX-License-Identifier: GPL-2.0-only
#

declare_platform(rk3568 KernelPlatformRock3b PLAT_rk3568 KernelArchARM)

if(KernelPlatformRock3b)
    declare_seL4_arch(aarch64)
    set(KernelArmCortexA55 ON)
    set(KernelArchArmV8a ON)
    set(KernelArmGicV3 ON)
    config_set(KernelARMPlatform ARM_PLAT rock3b)
    list(APPEND KernelDTSList "tools/dts/rock3b.dts")
    list(APPEND KernelDTSList "src/plat/rk3568/overlay-rock3b.dts")

    declare_default_headers(
        TIMER_FREQUENCY 24000000
        MAX_IRQ 283
        NUM_PPI 32
        TIMER drivers/timer/arm_generic.h
        INTERRUPT_CONTROLLER arch/machine/gic_v3.h
        KERNEL_WCET 10u
    )
endif()

add_sources(
    DEP "KernelPlatformRock3b"
    CFILES src/arch/arm/machine/gic_v3.c src/arch/arm/machine/l2c_nop.c
)
