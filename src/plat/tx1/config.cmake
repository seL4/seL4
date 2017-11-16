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
endif()

add_sources(
    DEP "KernelPlatformTx1"
    CFILES
        src/plat/tx1/machine/hardware.c
        src/plat/tx1/machine/io.c
        src/plat/tx1/machine/l2cache.c
        src/arch/arm/machine/generic_timer.c
)
