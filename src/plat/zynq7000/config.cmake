#
# Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
#
# SPDX-License-Identifier: GPL-2.0-only
#

cmake_minimum_required(VERSION 3.7.2)

declare_platform(
    "zynq7000"
    "aarch32"
    MACH
    "zynq"
    # use default DTS at tools/dts/zynq7000.dts
    CAMKE_VAR
    "KernelPlatformZynq7000"
    # C_DEFINE defaults to CONFIG_PLAT_ZYNQ7000
    SOURCES
    "src/arch/arm/machine/l2c_310.c"
    "src/arch/arm/machine/gic_v2.c"
)

if(KernelPlatformZynq7000)

    set(KernelArmCortexA9 ON)
    set(KernelArchArmV7a ON)

    if(KernelIsMCS)
        list(APPEND KernelDTSList "${CMAKE_CURRENT_LIST_DIR}/mcs-overlay-zynq7000.dts")
        set(timer_file drivers/timer/arm_global.h)
        # This is the timer frequency that can pass tests (in particular
        # SCHED0011), but may not be the correct number.
        set(timer_freq 320000000llu)
    else()
        list(APPEND KernelDTSList "${CMAKE_CURRENT_LIST_DIR}/overlay-zynq7000.dts")
        set(timer_file drivers/timer/arm_priv.h)
        set(timer_freq 320000000llu)
    endif()

    declare_default_headers(
        TIMER_FREQUENCY ${timer_freq}
        MAX_IRQ 92
        NUM_PPI 32
        INTERRUPT_CONTROLLER arch/machine/gic_v2.h
        TIMER ${timer_file}
        CLK_SHIFT 40llu
        CLK_MAGIC 3435973837llu
        KERNEL_WCET 10llu
    )
endif()
