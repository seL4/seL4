#
# Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
# Copyright 2022, Capgemini Engineering
#
# SPDX-License-Identifier: GPL-2.0-only
#

cmake_minimum_required(VERSION 3.7.2)

declare_platform(
    "imx8mq-evk"
    "aarch64;aarch32"
    MACH
    "imx"
    # use default DTS at tools/dts/<board-name>.dts
    CAMKE_VAR
    "KernelPlatformImx8mq-evk"
    C_DEFINE
    "PLAT_IMX8MQ_EVK"
    SOURCES
    "src/arch/arm/machine/gic_v3.c"
    "src/arch/arm/machine/l2c_nop.c"
)

declare_platform(
    "imx8mm-evk"
    "aarch64;aarch32"
    MACH "imx"
    # use default DTS at tools/dts/<board-name>.dts
    CAMKE_VAR "KernelPlatformImx8mm-evk"
    C_DEFINE "PLAT_IMX8MM_EVK"
    SOURCES
        "src/arch/arm/machine/gic_v3.c"
        "src/arch/arm/machine/l2c_nop.c"
)

if(KernelPlatformImx8mq-evk)
    config_set(KernelPlatImx8mq PLAT_IMX8MQ ON)
endif()

if(KernelPlatformImx8mq-evk OR KernelPlatformImx8mm-evk)
    set(KernelArmCortexA53 ON)
    set(KernelArchArmV8a ON)
    set(KernelArmGicV3 ON)

    list(APPEND KernelDTSList "${CMAKE_CURRENT_LIST_DIR}/overlay-${KernelPlatform}.dts")
    if(KernelSel4ArchAarch32)
        list(APPEND KernelDTSList "${CMAKE_CURRENT_LIST_DIR}/overlay-imx8m-32bit.dts")
    endif()

    declare_default_headers(
        TIMER_FREQUENCY 8000000
        MAX_IRQ 160
        TIMER drivers/timer/arm_generic.h
        INTERRUPT_CONTROLLER arch/machine/gic_v3.h
        NUM_PPI 32
        CLK_MAGIC 1llu
        CLK_SHIFT 3u
        KERNEL_WCET 10u
    )

endif()
