#
# Copyright 2019, Data61
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

declare_platform(imx8mq-evk KernelPlatformImx8mq-evk PLAT_IMX8MQ_EVK KernelArchARM)

if(KernelPlatformImx8mq-evk)
    if("${KernelSel4Arch}" STREQUAL aarch32)
        declare_seL4_arch(aarch32)
    elseif("${KernelSel4Arch}" STREQUAL aarch64)
        declare_seL4_arch(aarch64)
    else()
        message(
            STATUS "Selected platform ${KernelPlatform} supports multiple architectures but none were given"
        )
        message(STATUS "  Defaulting to aarch64")
        declare_seL4_arch(aarch64)
    endif()
    set(KernelArmCortexA53 ON)
    set(KernelArchArmV8a ON)
    config_set(KernelARMPlatform PLAT "imx8mq-evk")
    set(KernelArmMach "imx" CACHE INTERNAL "")
    set(KernelArmPASizeBits40 ON)
    list(APPEND KernelDTSList "tools/dts/imx8mq-evk.dts")
    list(APPEND KernelDTSList "src/plat/imx8mq-evk/overlay-imx8m.dts")
    if(KernelSel4ArchAarch32)
        list(APPEND KernelDTSList "src/plat/imx8mq-evk/overlay-imx8m-32bit.dts")
    endif()
    declare_default_headers(
        TIMER_FREQUENCY 8000000llu
        MAX_IRQ 160
        TIMER drivers/timer/arm_generic.h
        INTERRUPT_CONTROLLER arch/machine/gic_v3.h
    )
endif()

add_sources(
    DEP "KernelPlatformImx8mq-evk"
    CFILES src/arch/arm/machine/gic_v3.c src/arch/arm/machine/l2c_nop.c
)
