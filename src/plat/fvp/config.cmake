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

declare_platform(fvp KernelPlatformFVP-evk PLAT_FVP KernelSel4ArchAarch64)

if(KernelPlatformFVP)
    declare_seL4_arch(aarch64)
    set(KernelArmCortexA57 ON)
    set(KernelArchArmV8a ON)
    config_set(KernelPlatform PLAT "fvp")
    list(APPEND KernelDTSList "tools/dts/fvp.dts")
    declare_default_headers(
        TIMER_FREQUENCY 100000000llu
        MAX_IRQ 207
        INTERRUPT_CONTROLLER arch/machine/gic_v3.h
        TIMER drivers/timer/arm_priv.h
    )
endif()

add_sources(
    DEP "KernelPlatformFVP"
    CFILES src/arch/arm/machine/l2c_nop.c src/arch/arm/machine/gic_v3.c
)
