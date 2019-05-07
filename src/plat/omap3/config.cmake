#
# Copyright 2017, Data61
# Commonwealth Scientific and Industrial Research Organisation (CSIRO)
# ABN 41 687 119 230.
#
# This software may be distributed and modified according to the terms of
# the GNU General Public License version 2. Note that NO WARRANTY is provided.
# See "LICENSE_GPLv2.txt" for details.
#
# @TAG(DATA61_GPL)
#

cmake_minimum_required(VERSION 3.7.2)

declare_platform(omap3 KernelPlatformOMAP3 PLAT_OMAP3 KernelSel4ArchAarch32)

if(KernelPlatformOMAP3)
    declare_seL4_arch(aarch32)
    set(KernelArmCortexA8 ON)
    set(KernelArchArmV7a ON)
    config_set(KernelARMPlatform ARM_PLAT omap3)
    config_set(KernelArmMach MACH "omap")
    list(APPEND KernelDTSList "tools/dts/omap3.dts")
    list(APPEND KernelDTSList "src/plat/omap3/overlay-omap3.dts")
    declare_default_headers(
        TIMER_FREQUENCY 13000000llu
        MAX_IRQ 95
        INTERRUPT_CONTROLLER drivers/irq/omap3.h
        TIMER drivers/timer/omap3430.h
    )
endif()

add_sources(
    DEP "KernelPlatformOMAP3"
    CFILES src/plat/omap3/machine/hardware.c src/plat/omap3/machine/l2cache.c
)
