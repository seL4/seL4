#
# Copyright 2026, STMicroelectronics
#
# SPDX-License-Identifier: GPL-2.0-only
#

declare_platform(stm32mp2 KernelPlatformMP2 PLAT_STM32MP2 KernelSel4ArchAarch64)

set(c_configs PLAT_STM32MP25_EV1)
set(cmake_configs KernelPlatformMP25_EV1)
set(plat_lists stm32mp257f-ev1)
foreach(config IN LISTS cmake_configs)
    unset(${config} CACHE)
endforeach()

if(KernelPlatformMP2)
    declare_seL4_arch(aarch64)

    check_platform_and_fallback_to_default(KernelARMPlatform "stm32mp257f-ev1")

    list(FIND plat_lists ${KernelARMPlatform} index)
    if("${index}" STREQUAL "-1")
        message(FATAL_ERROR "Which stm32mp2 platform not specified ${KernelARMPlatform}")
    endif()

    list(GET c_configs ${index} c_config)
    list(GET cmake_configs ${index} cmake_config)
    config_set(${cmake_config} ${c_config} ON)
    list(APPEND KernelDTSList "tools/dts/${KernelARMPlatform}.dts")
    list(APPEND KernelDTSList "src/plat/stm32mp2/overlay-stm32mp2.dts")

    set(KernelArmCortexA35 ON)
    set(KernelArchArmV8a ON)
    set(KernelArmGicV2 ON)

    message(STATUS "KernelARMPlatform is ${KernelARMPlatform}")
    config_set(KernelARMPlatform ARM_PLAT ${KernelARMPlatform})
    config_set(KernelARMMach MACH "stm32mp2")

    declare_default_headers(
        TIMER_FREQUENCY 40000000
        NUM_PPI 32
        MAX_IRQ 415
        INTERRUPT_CONTROLLER arch/machine/gic_v2.h
        TIMER drivers/timer/arm_generic.h
        KERNEL_WCET 10u
    )
endif()

add_sources(
    DEP "KernelPlatformMP2"
    CFILES src/arch/arm/machine/gic_v2.c src/arch/arm/machine/l2c_nop.c
)
