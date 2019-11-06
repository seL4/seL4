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

declare_platform(imx8m KernelPlatImx8m PLAT_IMX8 KernelArchARM)

set(c_configs PLAT_IMX8MQ_EVK PLAT_IMX8MM_EVK)
set(cmake_configs KernelPlatformImx8mq KernelPlatformImx8mm)
set(plat_lists imx8mq-evk imx8mm-evk)
foreach(config IN LISTS cmake_configs)
    unset(${config} CACHE)
endforeach()
if(KernelPlatImx8m)
    if("${KernelSel4Arch}" STREQUAL aarch32)
        declare_seL4_arch(aarch32)
    elseif("${KernelSel4Arch}" STREQUAL aarch64)
        declare_seL4_arch(aarch64)
    else()
        message(
            STATUS
                "Selected platform ${KernelPlatform} supports multiple architectures but none were given"
        )
        message(STATUS "  Defaulting to aarch64")
        declare_seL4_arch(aarch64)
    endif()
    # MCS is not supported on imx8m.
    # It requires a timer driver that implements the tickless programming requirements.
    set(KernelPlatformSupportsMCS OFF)
    set(KernelArmCortexA53 ON)
    set(KernelArchArmV8a ON)
    set(KernelArmMach "imx" CACHE INTERNAL "")
    if("${KernelARMPlatform}" STREQUAL "")
        message(STATUS "Selected platform imx8 supports multiple sub platforms but none were given")
        message(STATUS "  Defaulting to imx8mq-evk")
        set(KernelARMPlatform imx8mq-evk)
    endif()
    list(FIND plat_lists ${KernelARMPlatform} index)
    if("${index}" STREQUAL "-1")
        message(FATAL_ERROR "Which imx8 platform not specified")
    endif()
    list(GET c_configs ${index} c_config)
    list(GET cmake_configs ${index} cmake_config)
    config_set(KernelARMPlatform ARM_PLAT ${KernelARMPlatform})
    config_set(${cmake_config} ${c_config} ON)

    list(APPEND KernelDTSList "tools/dts/${KernelARMPlatform}.dts")
    list(APPEND KernelDTSList "src/plat/imx8m-evk/overlay-${KernelARMPlatform}.dts")
    if(KernelSel4ArchAarch32)
        list(APPEND KernelDTSList "src/plat/imx8m-evk/overlay-imx8m-32bit.dts")
    endif()
    declare_default_headers(
        TIMER_FREQUENCY 8000000llu
        MAX_IRQ 160
        TIMER drivers/timer/arm_generic.h
        INTERRUPT_CONTROLLER arch/machine/gic_v3.h
        NUM_PPI 32
    )
endif()

add_sources(
    DEP "KernelPlatImx8m"
    CFILES src/arch/arm/machine/gic_v3.c src/arch/arm/machine/l2c_nop.c
)
