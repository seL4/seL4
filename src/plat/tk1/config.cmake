#
# Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
#
# SPDX-License-Identifier: GPL-2.0-only
#

cmake_minimum_required(VERSION 3.7.2)

declare_platform(
    "tk1"
    "aarch32;arm_hyp"
    MACH
    "nvidia"
    # use default DTS at tools/dts/<board-name>.dts
    CAMKE_VAR
    "KernelPlatformTK1"
    # C_DEFINE defaults to CONFIG_PLAT_TK1
    SOURCE
    "src/plat/tk1/machine/smmu.c"
    "src/arch/arm/machine/gic_v2.c"
    "src/arch/arm/machine/l2c_nop.c"
)

if(KernelPlatformTK1)
    set(KernelArmCortexA15 ON)
    set(KernelArchArmV7a ON)
    set(KernelArchArmV7ve ON)
    list(APPEND KernelDTSList "${CMAKE_CURRENT_LIST_DIR}/overlay-tk1.dts")
    declare_default_headers(
        TIMER_FREQUENCY 12000000
        MAX_IRQ 191
        INTERRUPT_CONTROLLER arch/machine/gic_v2.h
        NUM_PPI 32
        TIMER drivers/timer/arm_generic.h SMMU plat/machine/smmu.h
        CLK_MAGIC 2863311531llu
        CLK_SHIFT 35u
        KERNEL_WCET 100u
    )
endif()

add_bf_source_old("KernelPlatformTK1" "hardware.bf" "include/plat/tk1" "plat/machine")
