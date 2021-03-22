#
# Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
#
# SPDX-License-Identifier: GPL-2.0-only
#

cmake_minimum_required(VERSION 3.7.2)

declare_platform(
    "imx7"
    "aarch32"
    MACH
    "imx"
    # use default board specific DTS at tools/dts/<board-name>.dts
    CAMKE_VAR
    "KernelPlatImx7"  # ToDo: check who needs this?
    # C_DEFINE defaults to CONFIG_PLAT_IMX7
    SOURCES
    "src/arch/arm/machine/gic_v2.c"
    "src/arch/arm/machine/l2c_nop.c"
    BOARDS # first is default
    "imx7sabre,KernelPlatformImx7Sabre,PLAT_IMX7_SABRE"
)

if(KernelPlatformImx7Sabre)
    set(KernelArmCortexA7 ON)
    set(KernelArchArmV7a ON)
    list(APPEND KernelDTSList "${CMAKE_CURRENT_LIST_DIR}/overlay-imx7sabre.dts")
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
elseif(KernelPlatfrom_IMX7)
    message(FATAL_ERROR "cannot build imx7, use 'imx7sabre'")
endif()
