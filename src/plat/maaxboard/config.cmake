#
# Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
# Copyright 2022, Capgemini Engineering
#
# SPDX-License-Identifier: GPL-2.0-only
#

cmake_minimum_required(VERSION 3.7.2)

declare_platform(
    "maaxboard"
    "aarch64;aarch32" # default is first (aarch64)
    MACH
    "imx"
    # use default DTS at tools/dts/<board-name>.dts
    CAMKE_VAR
    "KernelPlatformMaaxboard" # default would be KernelPlatform_{name}
    # C_DEFINE defaults to CONFIG_PLAT_MAAXBOARD
    PLAT_CAMKE_VARS # all disabled by default, must be enabled explicitly
    "KernelPlatImx8mq"
    FLAGS
    "KernelArmCortexA53"
    "KernelArchArmV8a"
    "KernelArmGicV3"
    SOURCES
    "src/arch/arm/machine/gic_v3.c"
    "src/arch/arm/machine/l2c_nop.c"
)


if(KernelPlatformMaaxboard)

    config_set(KernelPlatImx8mq PLAT_IMX8MQ ON)

    add_kernel_dts("{CMAKE_CURRENT_LIST_DIR}/overlay-${KernelPlatform}.dts")
    if(KernelSel4ArchAarch32)
        add_kernel_dt("{CMAKE_CURRENT_LIST_DIR}/overlay-${KernelPlatform}-32bit.dts")
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

add_sources(
    DEP "KernelPlatformMaaxboard"
    CFILES src/arch/arm/machine/gic_v3.c src/arch/arm/machine/l2c_nop.c
)
