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

if(KernelPlatformTx1)
    set(KernelArmCortexA57 ON)
    set(KernelArchArmV8a ON)
    config_set(KernelPlatform PLAT "tx1")
    config_set(KernelArmMach MACH "nvidia")
    set(KernelHaveFPU ON)
    set(KernelArmPASizeBits44 ON)
    list(APPEND KernelDTSList "tools/dts/tx1.dts")
    list(APPEND KernelDTSList "src/plat/tx1/overlay-tx1.dts")
    declare_default_headers(19200000llu 224 arch/machine/gic_pl390.h arch/machine/generic_timer.h)
endif()

add_sources(
    DEP "KernelPlatformTx1"
    CFILES src/arch/arm/machine/gic_pl390.c src/arch/arm/machine/l2c_nop.c
)
