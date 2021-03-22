#
# Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
#
# SPDX-License-Identifier: GPL-2.0-only
#

cmake_minimum_required(VERSION 3.7.2)

declare_platform(
    "exynos4"
    "aarch32"
    MACH
    "exynos"
    # use default DTS at tools/dts/<board-name>.dts
    CAMKE_VAR
    "KernelPlatformExynos4"
    # C_DEFINE defaults to CONFIG_PLAT_EXYNOS4
    SOURCES
    "src/arch/arm/machine/l2c_310.c"
    "src/arch/arm/machine/gic_v2.c"
)

if(KernelPlatformExynos4)

    set(KernelArmCortexA9 ON)
    set(KernelArchArmV7a ON)

    list(APPEND KernelDTSList "${CMAKE_CURRENT_LIST_DIR}/overlay-exynos4.dts")

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
