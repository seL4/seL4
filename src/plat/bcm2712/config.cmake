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

    if(NOT DEFINED RPI5_MEMORY)
        # By default we assume an RPi5B model with 2GB of RAM
        # This is the minimum amount so ensures the kernel will boot on all
        # models.
        set(RPI5_MEMORY "2048")
    endif()

    if("${RPI5_MEMORY}" STREQUAL "2048")
        list(APPEND KernelDTSList "src/plat/bcm2712/overlay-rpi5-2gb.dts")
    elseif("${RPI5_MEMORY}" STREQUAL "4096")
        list(APPEND KernelDTSList "src/plat/bcm2712/overlay-rpi5-4gb.dts")
    elseif("${RPI5_MEMORY}" STREQUAL "8192")
        list(APPEND KernelDTSList "src/plat/bcm2712/overlay-rpi5-8gb.dts")
    elseif("${RPI5_MEMORY}" STREQUAL "16384")
        list(APPEND KernelDTSList "src/plat/bcm2712/overlay-rpi5-16gb.dts")
    else()
        message(FATAL_ERROR "Unsupported memory size given ${RPI5_MEMORY},
                            supported memory sizes (in megabytes) are 2048,
                            4096, 8192, and 16384.")
    endif()

    # - The clock frequency is 54 MHz as can be seen in bcm2712.dtsi in the
    #   Linux Kernel under clk_osc, thus TIMER_FREQUENCY = 54000000.
    # - The GIC ITLinesNumber reports 320 however there are not enough bits to
    #   encode the actual maximum IRQ so MAX_IRQ is based of the highest IRQ
    #   from the Device Tree which is from '/soc@107c000000/mmc@fff000'.
    declare_default_headers(
        TIMER_FREQUENCY 54000000
        MAX_IRQ 305
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
