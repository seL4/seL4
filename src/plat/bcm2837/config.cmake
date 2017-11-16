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

if(KernelPlatformRpi3)
    set(KernelArmCortexA53 ON)
    set(KernelArchArmV8a ON)
    config_set(KernelPlatform PLAT "bcm2837")
    set(KernelArmMachFeatureModifiers "+crc" CACHE INTERNAL "")
endif()

add_sources(
    DEP "KernelPlatformRpi3"
    CFILES
        src/plat/bcm2837/machine/hardware.c
        src/plat/bcm2837/machine/io.c
        src/plat/bcm2837/machine/intc.c
        src/plat/bcm2837/machine/l2cache.c
        src/arch/arm/machine/generic_timer.c
)
