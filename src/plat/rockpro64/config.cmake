#
# Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
#
# SPDX-License-Identifier: GPL-2.0-only
#

cmake_minimum_required(VERSION 3.7.2)

declare_platform(
    "rockpro64"
    "aarch64"
    # use default DTS at tools/dts/<board-name>.dts
    CAMKE_VAR
    "KernelPlatformRockpro64"
    # C_DEFINE defaults to CONFIG_PLAT_ROCKPRO64
    FLAGS
    "KernelArmCortexA53"
    "KernelArchArmV8a"
    "KernelArmGicV3"
    SOURCES
    "src/arch/arm/machine/gic_v3.c"
    "src/arch/arm/machine/l2c_nop.c"
)

if(KernelPlatformRockpro64)

    list(APPEND KernelDTSList "${CMAKE_CURRENT_LIST_DIR}/overlay-rockpro64.dts")

    declare_default_headers(
        TIMER_FREQUENCY 24000000
        MAX_IRQ 181
        NUM_PPI 32
        KERNEL_WCET 10u
        TIMER drivers/timer/arm_generic.h
        INTERRUPT_CONTROLLER arch/machine/gic_v3.h
    )

endif()
