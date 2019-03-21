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

if(KernelPlatformTK1)
    set(KernelArmCortexA15 ON)
    set(KernelArchArmV7a ON)
    set(KernelArchArmV7ve ON)
    config_set(KernelPlatform PLAT "tk1")
    config_set(KernelArmMach MACH "nvidia")
    list(APPEND KernelDTSList "tools/dts/tk1.dts")
    list(APPEND KernelDTSList "src/plat/tk1/overlay-tk1.dts")
endif()

add_sources(
    DEP "KernelPlatformTK1"
    CFILES
        src/plat/tk1/machine/smmu.c src/arch/arm/machine/gic_pl390.c src/arch/arm/machine/l2c_nop.c
)

add_bf_source_old("KernelPlatformTK1" "hardware.bf" "include/plat/tk1" "plat/machine")
