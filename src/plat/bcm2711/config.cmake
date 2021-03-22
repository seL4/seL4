#
# Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
# Copyright (C) 2021, Hensoldt Cyber GmbH
#
# SPDX-License-Identifier: GPL-2.0-only
#

cmake_minimum_required(VERSION 3.7.2)

declare_platform(
    "bcm2711"
    "aarch64;aarch32" # default is first (aarch64)
    # use default board specific DTS at tools/dts/<board-name>.dts
    # CAMKE_VAR defaults to KernelPlatform_BCM2711
    # C_DEFINE defaults to CONFIG_PLAT_BCM2711
    SOURCES
    "src/arch/arm/machine/gic_v2.c"
    "src/arch/arm/machine/l2c_nop.c"
    BOARDS
    "rpi4,KernelPlatformRpi4" # creates PLAT_BCM2711
)

if(KernelPlatformRpi4)
    set(KernelArmCortexA72 ON)
    set(KernelArchArmV8a ON)
    set(KernelArmMachFeatureModifiers "+crc" CACHE INTERNAL "")
    list(
        APPEND
        KernelDTSList
        "${CMAKE_CURRENT_LIST_DIR}/overlay-rpi4.dts"
        "${CMAKE_CURRENT_LIST_DIR}/overlay-rpi4-address-mapping.dts"
    )

    # - The clock frequency is 54 MHz as can be seen in bcm2711.dtsi in the
    # Linux Kernel under clk_osc, thus TIMER_FREQUENCY = 54000000.
    # - The GIC-400 offers 216 SPI IRQs (MAX_IRQ = 216) as can be seen in the
    # BCM2711 TRM under chapter 6.3. The GIC-400 implements the GICv2
    # architecture.
    # https://www.raspberrypi.org/forums/viewtopic.php?t=244479&start=25#p1499052
    # - CLK_MAGIC and CLK_SHIFT can be calculated with:
    #       tools/reciprocal.py --divisor 54000000
    declare_default_headers(
        TIMER_FREQUENCY 54000000
        MAX_IRQ 216
        NUM_PPI 32
        TIMER drivers/timer/arm_generic.h
        INTERRUPT_CONTROLLER arch/machine/gic_v2.h
        KERNEL_WCET 10u
        CLK_MAGIC 5337599559llu
        CLK_SHIFT 58u
    )
elseif(KernelPlatform_BCM2711)
    message(FATAL_ERROR "cannot build target 'bcm2711', use 'rpi4'")
endif()
