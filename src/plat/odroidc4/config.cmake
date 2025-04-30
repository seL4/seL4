#
# Copyright 2021, Data61, CSIRO (ABN 41 687 119 230)
#
# SPDX-License-Identifier: GPL-2.0-only
#

declare_platform(odroidc4 KernelPlatformOdroidc4 PLAT_ODROIDC4 KernelSel4ArchAarch64)

if(KernelPlatformOdroidc4)
    declare_seL4_arch(aarch64)
    set(KernelArmCortexA55 ON)
    set(KernelArchArmV8a ON)
    config_set(KernelARMPlatform ARM_PLAT odroidc4)
    set(KernelArmMachFeatureModifiers "+fp+simd+crc" CACHE INTERNAL "")
    list(APPEND KernelDTSList "tools/dts/odroidc4.dts" "src/plat/odroidc4/overlay-odroidc4.dts")
    # MAX_IRQ is based on the section 7.10.2 of the S905X3 SoC manual
    declare_default_headers(
        TIMER_FREQUENCY 24000000
        MAX_IRQ 255
        NUM_PPI 32
        TIMER drivers/timer/arm_generic.h
        INTERRUPT_CONTROLLER arch/machine/gic_v2.h
        CLK_MAGIC 375299969u
        CLK_SHIFT 53u
        KERNEL_WCET 10u
        TIMER_PRECISION 1u
    )
endif()

add_sources(
    DEP "KernelPlatformOdroidc4"
    CFILES src/arch/arm/machine/gic_v2.c src/arch/arm/machine/l2c_nop.c
)
