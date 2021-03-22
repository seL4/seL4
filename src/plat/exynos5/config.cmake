#
# Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
#
# SPDX-License-Identifier: GPL-2.0-only
#

cmake_minimum_required(VERSION 3.7.2)

declare_platform(
    "exynos5"
    "aarch32;arm_hyp"
    MACH
    "exynos"
    # use default DTS from board at tools/dts/<board-name>.dts
    CAMKE_VAR
    "KernelPlatExynos5"
    # C_DEFINE defaults to CONFIG_PLAT_EXYNOS5
    SOURCES
    "src/arch/arm/machine/gic_v2.c"
    "src/arch/arm/machine/l2c_nop.c"
    BOARDS # first is default
    "exynos5250,KernelPlatformExynos5250" # creates PLAT_EXYNOS5250
    "exynos5410,KernelPlatformExynos5410" # creates PLAT_EXYNOS5410
    "exynos5422,KernelPlatformExynos5422" # creates PLAT_EXYNOS5422
)

if(KernelPlatExynos5)

    set(KernelArmCortexA15 ON)
    set(KernelArchArmV7ve ON)
    # v7ve is a superset of v7a, so we enable that as well
    set(KernelArchArmV7a ON)

    if(KernelPlatformExynos5410 OR KernelPlatformExynos5422)
        config_set(KernelPlatExynos54xx PLAT_EXYNOS54XX ON)
    elseif(KernelPlatformExynos5250)
        config_set(KernelPlatExynos54xx PLAT_EXYNOS54XX OFF)
    else()
        message(FATAL_ERROR "invalid exynos5 board")
    endif()

    if(NOT KernelPlatformExynos5422)
        set(KernelHardwareDebugAPIUnsupported ON CACHE INTERNAL "")
    endif()

    list(APPEND KernelDTSList "${CMAKE_CURRENT_LIST_DIR}/overlay-${KernelARMPlatform}.dts")

    declare_default_headers(
        TIMER_FREQUENCY 24000000
        MAX_IRQ 254
        NUM_PPI 32
        TIMER drivers/timer/arm_generic.h
        INTERRUPT_CONTROLLER arch/machine/gic_v2.h
        CLK_MAGIC 2863311531llu
        CLK_SHIFT 36u
        KERNEL_WCET 10u
    )
endif()
