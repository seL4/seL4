#
# Copyright 2018, Data61
# Commonwealth Scientific and Industrial Research Organisation (CSIRO)
# ABN 41 687 119 230.
#
# This software may be distributed and modified according to the terms of
# the GNU General Public License version 2. Note that NO WARRANTY is provided.
# See "LICENSE_GPLv2.txt" for details.
#
# @TAG(DATA61_GPL)
#

cmake_minimum_required(VERSION 3.7.2)

declare_platform(ultra96 KernelPlatformUltra96 PLAT_ZYNQMP_ULTRA96 KernelArchARM)
declare_platform(zynqmp KernelPlatformZynqmp PLAT_ZYNQMP KernelArchARM)

if(KernelPlatformUltra96)
    # Ultra96 is is basically Zynqmp
    list(APPEND KernelDTSList "tools/dts/ultra96.dts")
    config_set(KernelPlatformZynqmp PLAT_ZYNQMP ON)
endif()

if(KernelPlatformZynqmp)
    if("${KernelSel4Arch}" STREQUAL aarch32)
        declare_seL4_arch(aarch32)
    elseif("${KernelSel4Arch}" STREQUAL aarch64)
        declare_seL4_arch(aarch64)
    else()
        fallback_declare_seL4_arch_default(aarch64)
    endif()
    # MCS is not supported on zynqmp.
    # It requires a timer driver that implements the tickless programming requirements.
    set(KernelPlatformSupportsMCS OFF)
    set(KernelArmCortexA53 ON)
    set(KernelArchArmV8a ON)
    config_set(KernelARMPlatform ARM_PLAT zynqmp)
    config_set(KernelArmMach MACH "zynq")

    if(NOT KernelPlatformUltra96)
        list(APPEND KernelDTSList "tools/dts/zynqmp.dts")
    endif()
    list(APPEND KernelDTSList "src/plat/zynqmp/overlay-zynqmp.dts")

    declare_default_headers(
        TIMER_FREQUENCY 100000000llu
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
