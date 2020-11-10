#
# Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
#
# SPDX-License-Identifier: GPL-2.0-only
#

cmake_minimum_required(VERSION 3.7.2)

# We introduce a variable to hold this long expression to prevent the
# code styler from line-wrapping the declare_platform() statement.  We
# want to keep that on one line so the `griddle` tool (or humans) can
# easily `grep` a list of supported platforms. 
# For now we don't support HYP... but include it for later.
#set(AArch32OrArchArmHyp "KernelSel4ArchAarch32 OR KernelSel4ArchArmHyp")
set(AArch32OrArchArmHyp "KernelSel4ArchAarch32")
declare_platform(rk3288 KernelPlatformRK3288 PLAT_RK3288 ${AArch32OrArchArmHyp})
unset(${AArch32OrArchArmHyp} CACHE)

set(cmake_configs KernelPlatformRK3288NTablet)
set(c_configs PLAT_RK3288_NTABLET)
set(plat_lists rk3288-ntablet)
foreach(config IN LISTS cmake_configs)
    unset(${config} CACHE)
endforeach()

if(KernelPlatformRK3288)
#    if("${KernelSel4Arch}" STREQUAL aarch32)
     declare_seL4_arch(aarch32)
#    elseif("${KernelSel4Arch}" STREQUAL arm_hyp)
#        declare_seL4_arch(arm_hyp)
#    else()
#        fallback_declare_seL4_arch_default(aarch32)
#    endif()
    set(KernelArmCortexA15 ON) #almost the same as A17
    set(KernelArchArmV7ve ON)
    # v7ve is a superset of v7a, so we enable that as well
    set(KernelArchArmV7a ON)
    config_set(KernelArmMach MACH "rk3288")
    check_platform_and_fallback_to_default(KernelARMPlatform "rk3288-ntablet")

    list(FIND plat_lists "${KernelARMPlatform}" index)
    if("${index}" STREQUAL "-1")
        message(FATAL_ERROR "Invalid rk3288 platform selected: \"${KernelARMPlatform}\"")
    endif()
    list(GET c_configs ${index} c_config)
    list(GET cmake_configs ${index} cmake_config)
    config_set(KernelARMPlatform ARM_PLAT ${KernelARMPlatform})
    config_set(${cmake_config} ${c_config} ON)

    list(APPEND KernelDTSList "tools/dts/${KernelARMPlatform}.dts")
    list(APPEND KernelDTSList "src/plat/rk3288/overlay-${KernelARMPlatform}.dts")
    declare_default_headers(
        TIMER_FREQUENCY 24000000llu
        MAX_IRQ 186
        NUM_PPI 4
        TIMER drivers/timer/arm_generic.h
        INTERRUPT_CONTROLLER arch/machine/gic_v2.h
        CLK_MAGIC 2863311531llu
        CLK_SHIFT 36u
        KERNEL_WCET 10u
    )
endif()

add_sources(
    DEP "KernelPlatformRK3288"
    CFILES
        src/arch/arm/machine/gic_v2.c
        src/arch/arm/machine/l2c_nop.c
        src/plat/rk3288/machine/timer.c
)
