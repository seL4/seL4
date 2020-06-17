#
# Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
#
# SPDX-License-Identifier: GPL-2.0-only
#

cmake_minimum_required(VERSION 3.7.2)

declare_platform(imx6 KernelPlatImx6 PLAT_IMX6 KernelSel4ArchAarch32)

set(c_configs PLAT_SABRE PLAT_WANDQ)
set(cmake_configs KernelPlatformSabre KernelPlatformWandQ)
set(plat_lists sabre wandq)
foreach(config IN LISTS cmake_configs)
    unset(${config} CACHE)
endforeach()
if(KernelPlatImx6)
    declare_seL4_arch(aarch32)
    set(KernelArmCortexA9 ON)
    set(KernelArchArmV7a ON)
    set(KernelArmMach "imx" CACHE INTERNAL "")
    check_platform_and_fallback_to_default(KernelARMPlatform "sabre")
    list(FIND plat_lists ${KernelARMPlatform} index)
    if("${index}" STREQUAL "-1")
        message(FATAL_ERROR "Which imx6 platform not specified")
    endif()
    list(GET c_configs ${index} c_config)
    list(GET cmake_configs ${index} cmake_config)
    config_set(KernelARMPlatform ARM_PLAT ${KernelARMPlatform})
    config_set(${cmake_config} ${c_config} ON)
    list(APPEND KernelDTSList "tools/dts/${KernelARMPlatform}.dts")
    list(APPEND KernelDTSList "src/plat/imx6/overlay-${KernelARMPlatform}.dts")

    if(KernelIsMCS)
        list(APPEND KernelDTSList "src/plat/imx6/mcs-overlay-imx6.dts")
        set(timer_file drivers/timer/arm_global.h)
        set(timer_freq 498000000llu)
    else()
        set(timer_file drivers/timer/arm_priv.h)
        set(timer_freq 498000000llu)
    endif()

    declare_default_headers(
        TIMER_FREQUENCY ${timer_freq}
        MAX_IRQ 159
        INTERRUPT_CONTROLLER arch/machine/gic_v2.h
        NUM_PPI 32
        TIMER ${timer_file}
        CLK_SHIFT 41llu
        CLK_MAGIC 4415709349llu
        KERNEL_WCET 10llu
        TIMER_PRECISION 2u
    )
endif()

add_sources(
    DEP "KernelPlatImx6"
    CFILES src/arch/arm/machine/l2c_310.c src/arch/arm/machine/gic_v2.c
)
