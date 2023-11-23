#
# Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
# Copyright (C) 2021, Hensoldt Cyber GmbH
#
# SPDX-License-Identifier: GPL-2.0-only
#

cmake_minimum_required(VERSION 3.7.2)

declare_platform(bcm2711 KernelPlatformRpi4 PLAT_BCM2711 KernelArchARM)

if(KernelPlatformRpi4)
    declare_seL4_arch(aarch64 aarch32)
    set(KernelArmCortexA72 ON)
    set(KernelArchArmV8a ON)
    config_set(KernelARMPlatform ARM_PLAT rpi4)
    set(KernelArmMachFeatureModifiers "+crc" CACHE INTERNAL "")
    list(APPEND KernelDTSList "tools/dts/rpi4.dts")
    list(APPEND KernelDTSList "src/plat/bcm2711/overlay-rpi4.dts")
    list(APPEND KernelDTSList "src/plat/bcm2711/overlay-rpi4-address-mapping.dts")

    if(NOT DEFINED RPI4_MEMORY)
        # By default we assume an RPi4B model with 8GB of RAM
        set(RPI4_MEMORY "8192")
    endif()

    if("${RPI4_MEMORY}" STREQUAL "1024")
        list(APPEND KernelDTSList "src/plat/bcm2711/overlay-rpi4-1gb.dts")
    elseif("${RPI4_MEMORY}" STREQUAL "2048")
        list(APPEND KernelDTSList "src/plat/bcm2711/overlay-rpi4-2gb.dts")
    elseif("${RPI4_MEMORY}" STREQUAL "4096")
        list(APPEND KernelDTSList "src/plat/bcm2711/overlay-rpi4-4gb.dts")
    elseif("${RPI4_MEMORY}" STREQUAL "8192")
        list(APPEND KernelDTSList "src/plat/bcm2711/overlay-rpi4-8gb.dts")
    else()
        message(FATAL_ERROR "Unsupported memory size given ${RPI4_MEMORY},
                            supported memory sizes (in megabytes) are 1024,
                            2048, 4096, and 8192.")
    endif()

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
endif()

add_sources(
    DEP "KernelPlatformRpi4"
    CFILES src/arch/arm/machine/gic_v2.c src/arch/arm/machine/l2c_nop.c
)
