#
# Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
#
# SPDX-License-Identifier: GPL-2.0-only
#

cmake_minimum_required(VERSION 3.7.2)

declare_platform(
    "imx31"
    "aarch32"
    MACH
    "imx"
    # use default board specific DTS at tools/dts/<board-name>.dts
    # CAMKE_VAR defaults to KernelPlatform_IMX31
    # C_DEFINE defaults to CONFIG_PLAT_IMX31
    SOURCES
    "src/plat/imx31/machine/hardware.c"
    BOARDS # first is default
    # can't use the default KernelPlatform_kzm because the custom name
    # KernelPlatformKZM is referenced in
    #   libplatsupport/CMakeLists.txt
    #   seL4/cmake-tool/helpers/simulation.cmake
    "kzm,KernelPlatformKZM" # creates PLAT_KZM
)


if(KernelPlatformKZM)

    set(KernelArm1136JF_S ON)
    set(KernelArchArmV6 ON)
    set(KernelHardwareDebugAPIUnsupported ON CACHE INTERNAL "")

    list(APPEND KernelDTSList "${CMAKE_CURRENT_LIST_DIR}/overlay-kzm.dts")

    if(KernelIsMCS)
        list(APPEND KernelDTSList "${CMAKE_CURRENT_LIST_DIR}/mcs-overlay-kzm.dts")
        set(TimerFrequency 35000000llu) # 35MHz -- calculated by trial and error, roughly precise
        set(TimerDriver drivers/timer/imx31-gpt.h)
    else()
        set(TimerFrequency 32768llu)
        set(TimerDriver drivers/timer/imx31-epit.h)
        add_bf_source_old("KernelPlatformKZM" "imx31-epit.bf" "include" "drivers/timer")
    endif()

    declare_default_headers(
        TIMER_FREQUENCY ${TimerFrequency}
        MAX_IRQ 63
        INTERRUPT_CONTROLLER drivers/irq/imx31.h
        TIMER ${TimerDriver}
        KERNEL_WCET 10u
        CLK_SHIFT 38u
        CLK_MAGIC 7853654485llu
    )

elseif(KernelPlatform_imx31)
    message(FATAL_ERROR "invalid i.MX31 board")
endif()
