#
# Copyright 2017, Data61
# Commonwealth Scientific and Industrial Research Organisation (CSIRO)
# ABN 41 687 119 230.
#
# This software may be distributed and modified according to the terms of
# the GNU General Public License version 2. Note that NO WARRANTY is provided.
# See "LICENSE_GPLv2.txt" for details.
#
# @TAG(DATA61_GPL)
#

cmake_minimum_required(VERSION 3.7.2)

if(KernelPlatformExynos5250 OR KernelPlatformExynos5410 OR KernelPlatformExynos5422)
    set(KernelArmCortexA15 ON)
    set(KernelArchArmV7ve ON)
    # v7ve is a superset of v7a, so we enable that as well
    set(KernelArchArmV7a ON)
    config_set(KernelPlatform PLAT "exynos5")
    config_set(KernelArmMach MACH "exynos")
    config_set(KernelPlatExynos5 PLAT_EXYNOS5 ON)
    if(KernelPlatformExynos5410 OR KernelPlatformExynos5422)
        config_set(KernelPlatExynos54xx PLAT_EXYNOS54XX ON)
    endif()

    list(APPEND KernelDTSList "tools/dts/${KernelARMPlatform}.dts")
    list(APPEND KernelDTSList "src/plat/exynos5/overlay-${KernelARMPlatform}.dts")
    declare_default_headers(
        TIMER_FREQUENCY 24000000llu
        MAX_IRQ 232
        TIMER arch/machine/generic_timer.h
        INTERRUPT_CONTROLLER arch/machine/gic_pl390.h
    )
else()
    config_set(KernelPlatExynos5 PLAT_EXYNOS5 OFF)
    config_set(KernelPlatExynos54xx PLAT_EXYNOS54XX OFF)
endif()

add_sources(
    DEP "KernelPlatExynos5"
    CFILES src/arch/arm/machine/gic_pl390.c src/arch/arm/machine/l2c_nop.c
)
