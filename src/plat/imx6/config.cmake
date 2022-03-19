#
# Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
# Copyright 2020, HENSOLDT Cyber GmbH
#
# SPDX-License-Identifier: GPL-2.0-only
#

cmake_minimum_required(VERSION 3.7.2)

declare_platform(imx6 KernelPlatImx6 PLAT_IMX6 KernelSel4ArchAarch32)

# disable platform specific settings by default in cache, will be enabled below
# if active
foreach(
    var
    IN
    ITEMS
    KernelPlatformSabre
    KernelPlatformWandQ
    KernelPlatformNitrogen6SX
    KernelPlatImx6dq
    KernelPlatImx6sx
)
    unset(${var} CACHE)
    set(${var} OFF)
endforeach()

if(KernelPlatImx6)

    check_platform_and_fallback_to_default(KernelARMPlatform "sabre")

    if(KernelARMPlatform STREQUAL "sabre")
        config_set(KernelPlatformSabre PLAT_SABRE ON)
        config_set(KernelPlatImx6dq PLAT_IMX6DQ ON)

    elseif(KernelARMPlatform STREQUAL "wandq")
        config_set(KernelPlatformWandQ PLAT_WANDQ ON)
        config_set(KernelPlatImx6dq PLAT_IMX6DQ ON)

    elseif(KernelARMPlatform STREQUAL "nitrogen6sx")
        config_set(KernelPlatformNitrogen6SX PLAT_NITROGEN6SX ON)
        config_set(KernelPlatImx6sx PLAT_IMX6SX ON)

    else()
        message(FATAL_ERROR "Which imx6 platform not specified")
    endif()

    config_set(KernelARMPlatform ARM_PLAT ${KernelARMPlatform})
    declare_seL4_arch(aarch32)
    set(KernelArmCortexA9 ON)
    set(KernelArchArmV7a ON)
    set(KernelArmMach "imx" CACHE INTERNAL "")
    list(APPEND KernelDTSList "tools/dts/${KernelARMPlatform}.dts")
    list(APPEND KernelDTSList "src/plat/imx6/overlay-${KernelARMPlatform}.dts")

    if(KernelIsMCS)
        if(KernelARMPlatform STREQUAL "nitrogen6sx")
            list(APPEND KernelDTSList "src/plat/imx6/mcs-overlay-nitrogen6sx.dts")
        else()
            list(APPEND KernelDTSList "src/plat/imx6/mcs-overlay-imx6.dts")
        endif()
        set(timer_file drivers/timer/arm_global.h)
    else()
        set(timer_file drivers/timer/arm_priv.h)
    endif()

    declare_default_headers(
        TIMER_FREQUENCY 498000000
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
