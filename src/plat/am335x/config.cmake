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

declare_platform(am335x KernelPlatformAM335X PLAT_AM335X KernelSel4ArchAarch32)
set(c_configs PLAT_AM335X_BONEBLACK PLAT_AM335X_BONEBLUE)
set(cmake_configs KernelPlatformAM335XBoneBlack KernelPlatformAM335XBoneBlue)
set(plat_lists am335x-boneblack am335x-boneblue)
foreach(config IN LISTS cmake_configs)
    unset(${config} CACHE)
endforeach()

if(KernelPlatformAM335X)
    declare_seL4_arch(aarch32)
    set(KernelArmCortexA8 ON)
    set(KernelArchArmV7a ON)
    if("${KernelARMPlatform}" STREQUAL "")
        message(
            STATUS "Selected platform am335x supports multiple sub platforms but none were given"
        )
        message(STATUS "  Defaulting to am335x-boneblack")
        set(KernelARMPlatform am335x-boneblack)
    endif()
    list(FIND plat_lists ${KernelARMPlatform} index)
    if("${index}" STREQUAL "-1")
        message(FATAL_ERROR "Which am335x platform not specified")
    endif()

    list(GET c_configs ${index} c_config)
    list(GET cmake_configs ${index} cmake_config)
    config_set(KernelARMPlatform ARM_PLAT ${KernelARMPlatform})
    config_set(${cmake_config} ${c_config} ON)
    list(APPEND KernelDTSList "tools/dts/${KernelARMPlatform}.dts")
    if("${KernelARMPlatform}" STREQUAL "am335x-boneblack")
        list(APPEND KernelDTSList "src/plat/am335x/overlay-am335x-boneblack.dts")
    endif()
    list(APPEND KernelDTSList "src/plat/am335x/overlay-am335x.dts")

    declare_default_headers(
        TIMER_FREQUENCY 32768llu
        MAX_IRQ 127
        TIMER drivers/timer/am335x.h
        INTERRUPT_CONTROLLER drivers/irq/am335x.h
    )
endif()

add_sources(
    DEP "KernelPlatformAM335X"
    CFILES src/plat/am335x/machine/hardware.c src/plat/am335x/machine/l2cache.c
)
