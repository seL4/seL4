#
# Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
#
# SPDX-License-Identifier: GPL-2.0-only
#

cmake_minimum_required(VERSION 3.7.2)

declare_platform(am335x KernelPlatformAM335X PLAT_AM335X KernelSel4ArchAarch32)
set(c_configs PLAT_AM335X_BONEBLACK PLAT_AM335X_BONEBLUE PLAT_AM335X_BONE)
set(
    cmake_configs
    KernelPlatformAM335XBoneBlack
    KernelPlatformAM335XBoneBlue
    KernelPlatformAM335XBone
)
set(plat_lists am335x-boneblack am335x-boneblue am335x-bone)
foreach(config IN LISTS cmake_configs)
    unset(${config} CACHE)
endforeach()

if(KernelPlatformAM335X)
    declare_seL4_arch(aarch32)

    set(KernelHardwareDebugAPIUnsupported ON CACHE INTERNAL "")

    set(KernelArmCortexA8 ON)
    set(KernelArchArmV7a ON)
    check_platform_and_fallback_to_default(KernelARMPlatform "am335x-boneblack")
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
        MAX_IRQ 127
        TIMER drivers/timer/am335x.h
        INTERRUPT_CONTROLLER
            drivers/irq/am335x.h
            #  DMTIMER 2-7 have programmable CLKSRC.
            #  Currently Kernel timer is DMTIMER4 using CLK_M_OSC.
        TIMER_FREQUENCY 24000000
        CLK_MAGIC 2863311531llu
        CLK_SHIFT 36u
        KERNEL_WCET 10u
    )
endif()

add_sources(
    DEP "KernelPlatformAM335X"
    CFILES src/plat/am335x/machine/hardware.c src/plat/am335x/machine/l2cache.c
)
