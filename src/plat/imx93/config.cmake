#
# Copyright 2024, Indan Zupancic
#
# SPDX-License-Identifier: GPL-2.0-only
#
declare_platform(imx93 KernelPlatformIMX93 PLAT_IMX93 KernelArchARM)

if(KernelPlatformIMX93)
    declare_seL4_arch(aarch64)
    set(KernelArmCortexA55 ON)
    set(KernelArchArmV8a ON)
    set(KernelArmGicV3 ON)
    config_set(KernelARMPlatform ARM_PLAT ${KernelPlatform})
    list(APPEND KernelDTSList "tools/dts/${KernelPlatform}.dts")
    list(APPEND KernelDTSList "src/plat/imx93/overlay-${KernelPlatform}.dts")
    declare_default_headers(
        TIMER_FREQUENCY 24000000
        TIMER drivers/timer/arm_generic.h
        TIMER_OVERHEAD_TICKS 1
        NUM_PPI 32
        MAX_IRQ 254
        INTERRUPT_CONTROLLER arch/machine/gic_v3.h
        KERNEL_WCET 10u
    )
endif()

add_sources(
    DEP "KernelPlatformIMX93"
    CFILES src/arch/arm/machine/gic_v3.c src/arch/arm/machine/l2c_nop.c
)
