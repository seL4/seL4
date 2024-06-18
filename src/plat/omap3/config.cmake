#
# Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
#
# SPDX-License-Identifier: GPL-2.0-only
#

cmake_minimum_required(VERSION 3.7.2)

declare_platform(omap3 KernelPlatformOMAP3 PLAT_OMAP3 KernelSel4ArchAarch32)

if(KernelPlatformOMAP3)
    declare_seL4_arch(aarch32)
    set(KernelArmCortexA8 ON)
    set(KernelArchArmV7a ON)
    set(KernelHardwareDebugAPIUnsupported ON CACHE INTERNAL "")
    config_set(KernelARMPlatform ARM_PLAT omap3)
    config_set(KernelArmMach MACH "omap")
    list(APPEND KernelDTSList "tools/dts/omap3.dts")
    list(APPEND KernelDTSList "src/plat/omap3/overlay-omap3.dts")
    declare_default_headers(
        TIMER_FREQUENCY 13000000
        MAX_IRQ 95
        INTERRUPT_CONTROLLER drivers/irq/omap3.h
        TIMER drivers/timer/omap3430.h
        CLK_MAGIC 1321528399llu
        CLK_SHIFT 34u
        KERNEL_WCET 10u
    )
endif()

add_sources(
    DEP "KernelPlatformOMAP3"
    CFILES src/plat/omap3/machine/hardware.c src/plat/omap3/machine/l2cache.c
)
