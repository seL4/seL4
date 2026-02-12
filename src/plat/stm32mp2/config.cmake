#
# Copyright 2026, STMicroelectronics
#
# SPDX-License-Identifier: GPL-2.0-only
#

declare_platform(stm32mp2 KernelPlatformMP2 PLAT_STM32MP2 KernelSel4ArchAarch64)

set(c_configs PLAT_STM32MP25_EV1)
set(
  cmake_configs
  KernelPlatformMP25_EV1
)
set(plat_lists stm32mp257f-ev1)
foreach(config IN LISTS cmake_configs)
  unset(${config} CACHE)
endforeach()

if(KernelPlatformMP2)
    declare_seL4_arch(aarch64)

    check_platform_and_fallback_to_default(KernelMP2Platform "stm32mp257f-ev1")

    list(FIND plat_lists ${KernelMP2Platform} index)
    if("${index}" STREQUAL "-1")
      message(FATAL_ERROR "Which stm32mp2 platform not specified ${KernelMP2Platform}")
    endif()

    list(GET c_configs ${index} c_config)
    list(GET cmake_configs ${index} cmake_config)
    config_set(KernelMP2Platform ARM_PLAT ${KernelMP2Platform})
    config_set(${cmake_config} ${c_config} ON)
    list(APPEND KernelDTSList "tools/dts/${KernelMP2Platform}.dts")
    list(APPEND KernelDTSList "src/plat/stm32mp2/overlay-stm32mp2.dts")

    set(KernelArmCortexA35 ON)
    set(KernelArchArmV8a ON)
    set(KernelArmGicV2 ON)
    set(KernelArmExportPCNTUser ON)

    message(STATUS "KernelMP2Platform is ${KernelMP2Platform}")
    config_set(KernelARMPlatform MACH "stm32mp2")

    declare_default_headers(
        TIMER_FREQUENCY 40000000
        MAX_IRQ 383
        INTERRUPT_CONTROLLER arch/machine/gic_v2.h
        NUM_PPI 32
        TIMER drivers/timer/arm_generic.h
    )
endif()

add_sources(
    DEP "KernelPlatformMP2"
    CFILES src/arch/arm/machine/gic_v2.c src/arch/arm/machine/l2c_nop.c
)
