#
# Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
#
# SPDX-License-Identifier: GPL-2.0-only
#

cmake_minimum_required(VERSION 3.7.2)

declare_platform(tx1 KernelPlatformTx1 PLAT_TX1 KernelSel4ArchAarch64)

if(KernelPlatformTx1)
    declare_seL4_arch(aarch64)
    set(KernelArmCortexA57 ON)
    set(KernelArchArmV8a ON)
    config_set(KernelARMPlatform ARM_PLAT tx1)
    config_set(KernelArmMach MACH "nvidia")
    list(APPEND KernelDTSList "tools/dts/tx1.dts")
    list(APPEND KernelDTSList "src/plat/tx1/overlay-tx1.dts")
    declare_default_headers(
        TIMER_FREQUENCY 12000000
        MAX_IRQ 224
        INTERRUPT_CONTROLLER arch/machine/gic_v2.h
        NUM_PPI 32
        TIMER drivers/timer/arm_generic.h
        CLK_MAGIC 2863311531llu
        CLK_SHIFT 35u
        KERNEL_WCET 10u
    )
endif()

add_sources(
    DEP "KernelPlatformTx1"
    CFILES src/arch/arm/machine/gic_v2.c src/arch/arm/machine/l2c_nop.c
)
