#
# Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
# Copyright 2022, Capgemini Engineering
#
# SPDX-License-Identifier: GPL-2.0-only
#

declare_platform(imx8mq-evk KernelPlatformImx8mq-evk PLAT_IMX8MQ_EVK KernelArchARM)
declare_platform(imx8mm-evk KernelPlatformImx8mm-evk PLAT_IMX8MM_EVK KernelArchARM)
declare_platform(imx8mp-evk KernelPlatformImx8mp-evk PLAT_IMX8MP_EVK KernelArchARM)

if(KernelPlatformImx8mq-evk OR KernelPlatformImx8mm-evk OR KernelPlatformImx8mp-evk)
    declare_seL4_arch(aarch64 aarch32)
    if(KernelPlatformImx8mq-evk)
        config_set(KernelPlatImx8mq PLAT_IMX8MQ ON)
    endif()
    if(KernelPlatformImx8mp-evk)
        # The i.MX 8M Plus SoC has higher interrupt numbers than the 8M Mini and the 8M Quad
        set(IMX8M_MAX_IRQ 192 CACHE INTERNAL "")
    else()
        set(IMX8M_MAX_IRQ 160 CACHE INTERNAL "")
    endif()
    set(KernelArmCortexA53 ON)
    set(KernelArchArmV8a ON)
    set(KernelArmGicV3 ON)
    config_set(KernelARMPlatform ARM_PLAT ${KernelPlatform})
    set(KernelArmMach "imx" CACHE INTERNAL "")
    list(APPEND KernelDTSList "tools/dts/${KernelPlatform}.dts")
    list(APPEND KernelDTSList "src/plat/imx8m-evk/overlay-${KernelPlatform}.dts")
    if(KernelSel4ArchAarch32)
        list(APPEND KernelDTSList "src/plat/imx8m-evk/overlay-imx8m-32bit.dts")
    endif()
    declare_default_headers(
        TIMER_FREQUENCY 8000000
        MAX_IRQ ${IMX8M_MAX_IRQ}
        TIMER drivers/timer/arm_generic.h
        INTERRUPT_CONTROLLER arch/machine/gic_v3.h
        NUM_PPI 32
        CLK_MAGIC 1llu
        CLK_SHIFT 3u
        KERNEL_WCET 10u
    )
endif()

add_sources(
    DEP "KernelPlatformImx8mq-evk OR KernelPlatformImx8mm-evk OR KernelPlatformImx8mp-evk"
    CFILES src/arch/arm/machine/gic_v3.c src/arch/arm/machine/l2c_nop.c
)
