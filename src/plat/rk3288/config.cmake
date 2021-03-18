#
# Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
#
# SPDX-License-Identifier: GPL-2.0-only
#

cmake_minimum_required(VERSION 3.7.2)

declare_platform(rk3288 KernelPlatRK3288 PLAT_RK3288 "KernelSel4ArchAarch32 OR KernelSel4ArchArmHyp")

if(KernelPlatRK3288)
    if("${KernelSel4Arch}" STREQUAL aarch32)
        declare_seL4_arch(aarch32)
	elseif("${KernelSel4Arch}" STREQUAL arm_hyp)
        declare_seL4_arch(arm_hyp)
    else()
        fallback_declare_seL4_arch_default(aarch32)
    endif()
    set(KernelArmCortexA15 ON) #almost the same as A17
    set(KernelArchArmV7ve ON)
    # v7ve is a superset of v7a, so we enable that as well
    set(KernelArchArmV7a ON)
    config_set(KernelARMPlatform ARM_PLAT rk3288)
    config_set(KernelArmMach "rk3288" CACHE INTERNAL "")

    list(APPEND KernelDTSList "tools/dts/${KernelARMPlatform}-ntablet.dts")
    list(APPEND KernelDTSList "src/plat/rk3288/overlay-${KernelARMPlatform}-ntablet.dts")

    declare_default_headers(
        TIMER_FREQUENCY 24000000llu
        MAX_IRQ 187
        NUM_PPI 32
        TIMER drivers/timer/arm_generic.h
        INTERRUPT_CONTROLLER arch/machine/gic_v2.h
        CLK_MAGIC 2863311531llu
        CLK_SHIFT 36u
        KERNEL_WCET 10u
    )
endif()

add_sources(
    DEP "KernelPlatRK3288"
    CFILES
        # I consider the next line to be too long,
        # but the github style checker insists on having it all on one line.
        src/arch/arm/machine/gic_v2.c src/arch/arm/machine/l2c_nop.c src/plat/rk3288/machine/timer.c
)
