#
# Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
#
# SPDX-License-Identifier: GPL-2.0-only
#

cmake_minimum_required(VERSION 3.7.2)

declare_platform(
    "bcm2837"
    "aarch32;aarch64" # default is first (aarch32)
    # use default board specific DTS at tools/dts/<board-name>.dts
    # CAMKE_VAR defaults to KernelPlatform_BCM2837
    # C_DEFINE defaults to CONFIG_PLAT_BCM2837
    SOURCES
    "src/plat/bcm2837/machine/intc.c"
    "src/arch/arm/machine/l2c_nop.c"
    BOARDS
    "rpi3,KernelPlatformRpi3"
)

if(KernelPlatformRpi3)
    set(KernelArmCortexA53 ON)
    set(KernelArchArmV8a ON)
    set(KernelArmMachFeatureModifiers "+crc" CACHE INTERNAL "")

    list(APPEND KernelDTSList "${CMAKE_CURRENT_LIST_DIR}/overlay-rpi3.dts")

    declare_default_headers(
        TIMER_FREQUENCY 19200000
        MAX_IRQ 127
        NUM_PPI 32
        TIMER drivers/timer/arm_generic.h
        INTERRUPT_CONTROLLER drivers/irq/bcm2836-armctrl-ic.h
        KERNEL_WCET 10u
        CLK_MAGIC 458129845llu
        CLK_SHIFT 43u
    )
elseif(KernelPlatform_BCM2837)
    message(FATAL_ERROR "cannot build target 'bcm2837', use 'rpi3'")
endif()
