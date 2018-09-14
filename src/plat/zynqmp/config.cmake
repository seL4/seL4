#
# Copyright 2018, Data61
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

if(KernelPlatformUltra96)
    # Ultra96 is is basically Zynqmp
    config_set(KernelPlatformZynqmp PLAT_ZYNQMP ON)
    config_set(KernelPlatformZynqmpUltra96 PLAT_ZYNQMP_ULTRA96 ON)
endif()

if(KernelPlatformZynqmp)
    set(KernelArmCortexA53 ON)
    set(KernelArchArmV8a ON)
    config_set(KernelPlatform PLAT "zynqmp")
    config_set(KernelArmMach MACH "zynq")
    if(KernelSel4ArchAarch64)
        set(KernelHaveFPU ON)
    endif()
endif()

add_sources(
    DEP "KernelPlatformZynqmp"
    CFILES
        src/plat/zynqmp/machine/hardware.c
        src/plat/zynqmp/machine/io.c
        src/arch/arm/machine/generic_timer.c
)
