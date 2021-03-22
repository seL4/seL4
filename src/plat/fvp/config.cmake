#
# Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
#
# SPDX-License-Identifier: GPL-2.0-only
#

cmake_minimum_required(VERSION 3.7.2)

declare_platform(
    "fvp"
    "aarch64"
    # use default DTS at tools/dts/<board-name>.dts
    CAMKE_VAR
    "KernelPlatformFVP"
    # C_DEFINE defaults to CONFIG_PLAT_FVP
    SOURCES
    "src/arch/arm/machine/l2c_nop.c"
    "src/arch/arm/machine/gic_v3.c"
)

if(KernelPlatformFVP)

    set(KernelArmCortexA57 ON)
    set(KernelArchArmV8a ON)
    set(KernelArmGicV3 ON)

    list(APPEND KernelDTSList "${CMAKE_CURRENT_LIST_DIR}/overlay-fvp.dts")

    declare_default_headers(
        TIMER_FREQUENCY 100000000
        MAX_IRQ 207
        INTERRUPT_CONTROLLER arch/machine/gic_v3.h
        TIMER drivers/timer/arm_generic.h
    )

endif()
