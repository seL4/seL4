#
# Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
#
# SPDX-License-Identifier: GPL-2.0-only
#

cmake_minimum_required(VERSION 3.7.2)

# We introduce a variable to hold this long expression to prevent the
# code styler from line-wrapping the declare_platform() statement.  We
# want to keep that on one line so the `griddle` tool (or humans) can
# easily `grep` a list of supported platforms.  As of 2019-08-07, this
# platform is the only one requiring this workaround.
set(AArch32OrArchArmHyp "KernelSel4ArchAarch32 OR KernelSel4ArchArmHyp")
declare_platform(exynos5 KernelPlatExynos5 PLAT_EXYNOS5 ${AArch32OrArchArmHyp})
unset(${AArch32OrArchArmHyp} CACHE)

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
        fallback_declare_seL4_arch_default(aarch32)
    endif()
    set(KernelArmCortexA15 ON)
    set(KernelArchArmV7ve ON)
    # v7ve is a superset of v7a, so we enable that as well
    set(KernelArchArmV7a ON)
    config_set(KernelArmMach MACH "exynos")
    check_platform_and_fallback_to_default(KernelARMPlatform "exynos5250")

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

    if(NOT KernelPlatformExynos5422)
        set(KernelHardwareDebugAPIUnsupported ON CACHE INTERNAL "")
    endif()

    list(APPEND KernelDTSList "tools/dts/${KernelARMPlatform}.dts")
    list(APPEND KernelDTSList "src/plat/exynos5/overlay-${KernelARMPlatform}.dts")
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

add_sources(
    DEP "KernelPlatExynos5"
    CFILES src/arch/arm/machine/gic_v2.c src/arch/arm/machine/l2c_nop.c
)
