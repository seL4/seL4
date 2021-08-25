#
# Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
#
# SPDX-License-Identifier: GPL-2.0-only
#

cmake_minimum_required(VERSION 3.7.2)

declare_platform(bcm2837 KernelPlatformRpi3 PLAT_BCM2837 KernelArchARM)

if(KernelPlatformRpi3)
    if("${KernelSel4Arch}" STREQUAL aarch32)
        declare_seL4_arch(aarch32)
    elseif("${KernelSel4Arch}" STREQUAL aarch64)
        declare_seL4_arch(aarch64)
    else()
        fallback_declare_seL4_arch_default(aarch32)
    endif()
    set(KernelArmCortexA53 ON)
    set(KernelArchArmV8a ON)
    config_set(KernelARMPlatform ARM_PLAT rpi3)
    set(KernelArmMachFeatureModifiers "+crc" CACHE INTERNAL "")
    list(APPEND KernelDTSList "tools/dts/rpi3.dts")
    list(APPEND KernelDTSList "src/plat/bcm2837/overlay-rpi3.dts")

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
endif()

add_sources(
    DEP "KernelPlatformRpi3"
    CFILES src/plat/bcm2837/machine/intc.c src/arch/arm/machine/l2c_nop.c
)
