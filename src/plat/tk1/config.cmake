#
# Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
#
# SPDX-License-Identifier: GPL-2.0-only
#

cmake_minimum_required(VERSION 3.7.2)

declare_platform(tk1 KernelPlatformTK1 PLAT_TK1 "KernelSel4ArchAarch32 OR KernelSel4ArchArmHyp")

if(KernelPlatformTK1)
    if("${KernelSel4Arch}" STREQUAL aarch32)
        declare_seL4_arch(aarch32)
    elseif("${KernelSel4Arch}" STREQUAL arm_hyp)
        declare_seL4_arch(arm_hyp)
    else()
        fallback_declare_seL4_arch_default(aarch32)
    endif()
    set(KernelArmCortexA15 ON)
    set(KernelArchArmV7a ON)
    set(KernelArchArmV7ve ON)
    config_set(KernelARMPlatform ARM_PLAT tk1)
    config_set(KernelArmMach MACH "nvidia")
    list(APPEND KernelDTSList "tools/dts/tk1.dts")
    list(APPEND KernelDTSList "src/plat/tk1/overlay-tk1.dts")
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

add_sources(
    DEP "KernelPlatformTK1"
    CFILES src/plat/tk1/machine/smmu.c src/arch/arm/machine/gic_v2.c src/arch/arm/machine/l2c_nop.c
)

add_bf_source_old("KernelPlatformTK1" "hardware.bf" "include/plat/tk1" "plat/machine")
