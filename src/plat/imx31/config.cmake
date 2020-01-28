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

declare_platform(imx31 KernelPlatformKZM PLAT_KZM KernelSel4ArchAarch32)

if(KernelPlatformKZM)
    declare_seL4_arch(aarch32)
    set(KernelArm1136JF_S ON)
    set(KernelArchArmV6 ON)
    set(KernelHardwareDebugAPIUnsupported ON CACHE INTERNAL "")
    config_set(KernelARMPlatform ARM_PLAT kzm)
    set(KernelArmMach "imx" CACHE INTERNAL "")
    list(APPEND KernelDTSList "tools/dts/kzm.dts")
    list(APPEND KernelDTSList "src/plat/imx31/overlay-kzm.dts")
    if(KernelIsMCS)
        list(APPEND KernelDTSList "src/plat/imx31/mcs-overlay-kzm.dts")
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
endif()

add_sources(DEP "KernelPlatformKZM" CFILES src/plat/imx31/machine/hardware.c)
