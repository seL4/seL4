#
# Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
#
# SPDX-License-Identifier: GPL-2.0-only
#

cmake_minimum_required(VERSION 3.7.2)

declare_platform(zynq7000 KernelPlatformZynq7000 PLAT_ZYNQ7000 KernelSel4ArchAarch32)

if(KernelPlatformZynq7000)
    declare_seL4_arch(aarch32)
    set(KernelArmCortexA9 ON)
    set(KernelArchArmV7a ON)
    config_set(KernelARMPlatform ARM_PLAT zynq7000)
    config_set(KernelArmMach MACH "zynq")
    list(APPEND KernelDTSList "tools/dts/zynq7000.dts")
    if(KernelIsMCS)
        list(APPEND KernelDTSList "src/plat/zynq7000/mcs-overlay-zynq7000.dts")
        set(timer_file drivers/timer/arm_global.h)
    else()
        list(APPEND KernelDTSList "src/plat/zynq7000/overlay-zynq7000.dts")
        set(timer_file drivers/timer/arm_priv.h)
    endif()
    declare_default_headers(
        # This is the timer frequency that can pass tests (in particular
        # SCHED0011), but may not be the correct number.
        TIMER_FREQUENCY 320000000
        MAX_IRQ 92
        NUM_PPI 32
        INTERRUPT_CONTROLLER arch/machine/gic_v2.h
        TIMER ${timer_file}
        CLK_SHIFT 40llu
        CLK_MAGIC 3435973837llu
        KERNEL_WCET 10llu
    )
endif()

add_sources(
    DEP "KernelPlatformZynq7000"
    CFILES src/arch/arm/machine/l2c_310.c src/arch/arm/machine/gic_v2.c
)
