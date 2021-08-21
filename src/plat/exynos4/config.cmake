#
# Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
#
# SPDX-License-Identifier: GPL-2.0-only
#

cmake_minimum_required(VERSION 3.7.2)

declare_platform(exynos4 KernelPlatformExynos4 PLAT_EXYNOS4 KernelSel4ArchAarch32)

if(KernelPlatformExynos4)
    declare_seL4_arch(aarch32)
    set(KernelArmCortexA9 ON)
    set(KernelArchArmV7a ON)
    config_set(KernelARMPlatform ARM_PLAT exynos4)
    config_set(KernelArmMach MACH "exynos")
    list(APPEND KernelDTSList "tools/dts/exynos4.dts")
    list(APPEND KernelDTSList "src/plat/exynos4/overlay-exynos4.dts")
    declare_default_headers(
        TIMER_FREQUENCY 24000000
        MAX_IRQ 159
        NUM_PPI 32
        TIMER drivers/timer/exynos4412-mct.h
        INTERRUPT_CONTROLLER arch/machine/gic_v2.h
        KERNEL_WCET 10u
        CLK_MAGIC 2863311531llu
        CLK_SHIFT 36u
        TIMER_PRECISION 0u
    )
endif()

add_sources(
    DEP "KernelPlatformExynos4"
    CFILES src/arch/arm/machine/l2c_310.c src/arch/arm/machine/gic_v2.c
)
