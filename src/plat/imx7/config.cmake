#
# Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
#
# SPDX-License-Identifier: GPL-2.0-only
#

cmake_minimum_required(VERSION 3.7.2)

declare_platform(imx7 KernelPlatImx7 PLAT_IMX7_SABRE KernelSel4ArchAarch32)

if(KernelPlatImx7)
    config_set(KernelPlatImx7 PLAT_IMX7 ON)
    declare_seL4_arch(aarch32)
    set(KernelArmCortexA7 ON)
    set(KernelArchArmV7a ON)
    config_set(KernelARMPlatform ARM_PLAT imx7sabre)
    set(KernelArmMach "imx" CACHE INTERNAL "")
    list(APPEND KernelDTSList "tools/dts/imx7sabre.dts")
    list(APPEND KernelDTSList "src/plat/imx7/overlay-imx7sabre.dts")
    declare_default_headers(
        TIMER_FREQUENCY 8000000
        MAX_IRQ 159
        NUM_PPI 32
        TIMER drivers/timer/arm_generic.h
        INTERRUPT_CONTROLLER arch/machine/gic_v2.h
        CLK_MAGIC 1llu
        CLK_SHIFT 8u
        KERNEL_WCET 10u
    )
else()
    config_set(KernelPlatImx7 PLAT_IMX7 OFF)
endif()

add_sources(
    DEP "KernelPlatImx7"
    CFILES src/arch/arm/machine/gic_v2.c src/arch/arm/machine/l2c_nop.c
)
