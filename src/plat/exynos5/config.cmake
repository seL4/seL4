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

declare_platform(
    exynos5
    KernelPlatExynos5
    PLAT_EXYNOS5
    "KernelSel4ArchAarch32 OR KernelSel4ArchArmHyp"
)

set(cmake_configs KernelPlatformExynos5250 KernelPlatformExynos5410 KernelPlatformExynos5422)
set(c_configs PLAT_EXYNOS5250 PLAT_EXYNOS5410 PLAT_EXYNOS5422)
set(plat_lists exynos5250 exynos5410 exynos5422)
foreach(config IN LISTS cmake_configs)
    unset(${config} CACHE)
endforeach()
unset(KernelPlatExynos54xx CACHE)
if(KernelPlatExynos5)
    if("${KernelSel4Arch}" STREQUAL aarch32)
        declare_seL4_arch(aarch32)
    elseif("${KernelSel4Arch}" STREQUAL arm_hyp)
        declare_seL4_arch(arm_hyp)
    else()
        message(
            STATUS "Selected platform exynos5 supports multiple architectures but none were given"
        )
        message(STATUS "  Defaulting to aarch32")
        declare_seL4_arch(aarch32)
    endif()
    set(KernelArmCortexA15 ON)
    set(KernelArchArmV7ve ON)
    # v7ve is a superset of v7a, so we enable that as well
    set(KernelArchArmV7a ON)
    config_set(KernelArmMach MACH "exynos")
    if("${KernelARMPlatform}" STREQUAL "")
        message(
            STATUS "Selected platform exynos5 supports multiple sub platforms but none were given"
        )
        message(STATUS "  Defaulting to exynos5250")
        set(KernelARMPlatform exynos5250)
    endif()

    list(FIND plat_lists "${KernelARMPlatform}" index)
    if("${index}" STREQUAL "-1")
        message(FATAL_ERROR "Invalid exynos5 platform selected: \"${KernelARMPlatform}\"")
    endif()
    list(GET c_configs ${index} c_config)
    list(GET cmake_configs ${index} cmake_config)
    config_set(KernelARMPlatform ARM_PLAT ${KernelARMPlatform})
    config_set(${cmake_config} ${c_config} ON)
    if(KernelPlatformExynos5410 OR KernelPlatformExynos5422)
        config_set(KernelPlatExynos54xx PLAT_EXYNOS54XX ON)
    else()
        config_set(KernelPlatExynos54xx PLAT_EXYNOS54XX OFF)
    endif()

    list(APPEND KernelDTSList "tools/dts/${KernelARMPlatform}.dts")
    list(APPEND KernelDTSList "src/plat/exynos5/overlay-${KernelARMPlatform}.dts")
    declare_default_headers(
        TIMER_FREQUENCY 24000000llu
        MAX_IRQ 232
        TIMER drivers/timer/arm_generic.h
        INTERRUPT_CONTROLLER arch/machine/gic_v2.h
    )
endif()

add_sources(
    DEP "KernelPlatExynos5"
    CFILES src/arch/arm/machine/gic_v2.c src/arch/arm/machine/l2c_nop.c
)
