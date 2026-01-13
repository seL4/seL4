#
# Copyright 2025, UNSW
#
# SPDX-License-Identifier: GPL-2.0-only
#

declare_platform(bcm2712 KernelPlatformRpi5 PLAT_BCM2712 KernelArchARM)

if(KernelPlatformRpi5)
    declare_seL4_arch(aarch64)
    set(KernelArmCortexA76 ON)
    set(KernelArchArmV8a ON)
    config_set(KernelARMPlatform ARM_PLAT rpi5)
    set(KernelArmMachFeatureModifiers "+crc" CACHE INTERNAL "")
    list(APPEND KernelDTSList "tools/dts/rpi5b.dts")
    list(APPEND KernelDTSList "src/plat/bcm2712/overlay-rpi5.dts")
    # The 2GB model is assumed, this can be changed with the KernelCustomDTSOverlay
    # configuration option or adding support for other models like is done for
    # bcm2711.
    list(APPEND KernelDTSList "src/plat/bcm2712/overlay-rpi5-2gb.dts")

    # - The clock frequency is 54 MHz as can be seen in bcm2712.dtsi in the
    #   Linux Kernel under clk_osc, thus TIMER_FREQUENCY = 54000000.
    # - MAX_IRQ is based on the GIC ITLinesNumber which reports 320.
    declare_default_headers(
        TIMER_FREQUENCY 54000000
        MAX_IRQ 320
        NUM_PPI 32
        TIMER drivers/timer/arm_generic.h
        INTERRUPT_CONTROLLER arch/machine/gic_v2.h
        KERNEL_WCET 10u
    )
endif()

add_sources(
    DEP "KernelPlatformRpi5"
    CFILES src/arch/arm/machine/gic_v2.c src/arch/arm/machine/l2c_nop.c
)
