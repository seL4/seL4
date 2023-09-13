#
# Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
#
# SPDX-License-Identifier: GPL-2.0-only
#

cmake_minimum_required(VERSION 3.7.2)

declare_platform(zynqmp KernelPlatformZynqmp PLAT_ZYNQMP KernelArchARM)

set(c_configs PLAT_ZYNQMP_ZCU102 PLAT_ZYNQMP_ULTRA96 PLAT_ZYNQMP_ULTRA96V2)
set(
    cmake_configs
    KernelPlatformZynqmpZcu102
    KernelPlatformZynqmpUltra96
    KernelPlatformZynqmpUltra96v2
)
set(plat_lists zcu102 ultra96 ultra96v2)
foreach(config IN LISTS cmake_configs)
    unset(${config} CACHE)
endforeach()

config_choice(
    KernelZynqmpPetalinuxVersion
    KERNEL_ZYNQMP_PETALINUX_VERSION
    "The version of Petalinux to use for the kernel device tree"
    "2018_3;KernelZynqmpPetalinux2018_3;ZYNQMP_PETALINUX_2018_3;KernelPlatformZynqmp"
    "2021_1;KernelZynqmpPetalinux2021_1;ZYNQMP_PETALINUX_2021_1;KernelPlatformZynqmp"
)

config_choice(
    KernelZynqmpBusName
    ZYNQMP_BUS_NAME
    "The bus name in the the kernel device tree (axi/amba)"
    "amba;KernelZynqmpPetalinuxAmbaBus;ZYNQMP_PETALINUX_AMBA;KernelPlatformZynqmp"
    "axi;KernelZynqmpPetalinuxAxiBus;ZYNQMP_PETALINUX_AXI;KernelPlatformZynqmp"
)

if(KernelZynqmpPetalinux2021_1)
    set(KernelZynqmpBusName "axi")
endif()

if(KernelPlatformZynqmp)
    set(KernelHardwareDebugAPIUnsupported ON CACHE INTERNAL "")
    if("${KernelSel4Arch}" STREQUAL aarch32)
        declare_seL4_arch(aarch32)
    elseif("${KernelSel4Arch}" STREQUAL aarch64)
        declare_seL4_arch(aarch64)
    elseif("${KernelSel4Arch}" STREQUAL arm_hyp)
        declare_seL4_arch(arm_hyp)
    else()
        fallback_declare_seL4_arch_default(aarch64)
    endif()

    check_platform_and_fallback_to_default(KernelARMPlatform "zcu102")

    list(FIND plat_lists ${KernelARMPlatform} index)
    if("${index}" STREQUAL "-1")
        message(FATAL_ERROR "Which zynqmp platform not specified")
    endif()
    list(GET c_configs ${index} c_config)
    list(GET cmake_configs ${index} cmake_config)
    config_set(KernelARMPlatform ARM_PLAT ${KernelARMPlatform})
    config_set(${cmake_config} ${c_config} ON)

    set(KernelArmCortexA53 ON)
    set(KernelArchArmV8a ON)

    config_set(KernelArmMach MACH "zynq")

    if(KernelPlatformZynqmpUltra96v2)
        list(APPEND KernelDTSList "tools/dts/${KernelZynqmpPetalinuxVersion}/ultra96v2.dts")
    elseif(KernelPlatformZynqmpUltra96)
        list(APPEND KernelDTSList "tools/dts/ultra96.dts")
    elseif(KernelPlatformZynqmpZcu102)
        list(APPEND KernelDTSList "tools/dts/${KernelZynqmpPetalinuxVersion}/zynqmp.dts")
    else()
        message(FATAL_ERROR "unknown platform")
    endif()

    if(KernelSel4ArchAarch32)
        list(
            APPEND
                KernelDTSList "src/plat/zynqmp/${KernelZynqmpPetalinuxVersion}/overlay-zynqmp32.dts"
        )
    else()
        list(
            APPEND
                KernelDTSList "src/plat/zynqmp/${KernelZynqmpPetalinuxVersion}/overlay-zynqmp.dts"
        )
    endif()

    declare_default_headers(
        TIMER_FREQUENCY 100000000
        MAX_IRQ 187
        NUM_PPI 32
        TIMER drivers/timer/arm_generic.h
        INTERRUPT_CONTROLLER arch/machine/gic_v2.h
        CLK_MAGIC 1374389535llu
        CLK_SHIFT 37u
        KERNEL_WCET 10u
    )
endif()

add_sources(
    DEP "KernelPlatformZynqmp"
    CFILES src/arch/arm/machine/gic_v2.c src/arch/arm/machine/l2c_nop.c
)
