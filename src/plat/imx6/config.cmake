#
# Copyright 2017, Data61
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

declare_platform(imx6 KernelPlatImx6 PLAT_IMX6 KernelSel4ArchAarch32)

set(c_configs PLAT_SABRE PLAT_WANDQ)
set(cmake_configs KernelPlatformSabre KernelPlatformWandQ)
set(plat_lists sabre wandq)
foreach(config IN LISTS cmake_configs)
    unset(${config} CACHE)
endforeach()
if(KernelPlatImx6)
    declare_seL4_arch(aarch32)
    set(KernelArmCortexA9 ON)
    set(KernelArchArmV7a ON)
    set(KernelArmMach "imx" CACHE INTERNAL "")
    if("${KernelARMPlatform}" STREQUAL "")
        message(STATUS "Selected platform imx6 supports multiple sub platforms but none were given")
        message(STATUS "  Defaulting to sabre")
        set(KernelARMPlatform sabre)
    endif()
    list(FIND plat_lists ${KernelARMPlatform} index)
    if("${index}" STREQUAL "-1")
        message(FATAL_ERROR "Which imx6 platform not specified")
    endif()
    list(GET c_configs ${index} c_config)
    list(GET cmake_configs ${index} cmake_config)
    config_set(KernelARMPlatform ARM_PLAT ${KernelARMPlatform})
    config_set(${cmake_config} ${c_config} ON)
    list(APPEND KernelDTSList "tools/dts/${KernelARMPlatform}.dts")
    list(APPEND KernelDTSList "src/plat/imx6/overlay-${KernelARMPlatform}.dts")
    declare_default_headers(
        TIMER_FREQUENCY 400000000llu
        MAX_IRQ 159
        INTERRUPT_CONTROLLER arch/machine/gic_v2.h
        NUM_PPI 32
        TIMER drivers/timer/arm_priv.h
    )
endif()

add_sources(
    DEP "KernelPlatImx6"
    CFILES src/arch/arm/machine/l2c_310.c src/arch/arm/machine/gic_v2.c
)
