#
# Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
#
# SPDX-License-Identifier: GPL-2.0-only
#

cmake_minimum_required(VERSION 3.7.2)

declare_platform(
    "odroidc2"
    "aarch64"
    # use default DTS at tools/dts/<board-name>.dts
    CAMKE_VAR
    "KernelPlatformOdroidc2"
    # C_DEFINE defaults to CONFIG_PLAT_ODROIDC2
    FLAGS
    "KernelArmCortexA53"
    "KernelArchArmV8a"
    SOURCES
    "src/arch/arm/machine/gic_v2.c"
    "src/arch/arm/machine/l2c_nop.c"
)

if(KernelPlatformOdroidc2)
    set(KernelArmMachFeatureModifiers "+crc" CACHE INTERNAL "")
    list(APPEND KernelDTSList "${CMAKE_CURRENT_LIST_DIR}/overlay-odroidc2.dts")
    declare_default_headers(
        TIMER_FREQUENCY 24000000
        MAX_IRQ 250
        NUM_PPI 32
        TIMER drivers/timer/arm_generic.h
        INTERRUPT_CONTROLLER arch/machine/gic_v2.h
        CLK_MAGIC 375299969u
        CLK_SHIFT 53u
        KERNEL_WCET 10u
        TIMER_PRECISION 1u
    )
endif()
