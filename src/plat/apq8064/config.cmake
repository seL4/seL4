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

if(KernelPlatformAPQ8064)
    set(KernelArmCortexA15 ON)
    set(KernelArchArmV7a ON)
    set(KernelArchArmV7ve ON)
    config_set(KernelPlatform PLAT "apq8064")
endif()

add_sources(
    DEP "KernelPlatformAPQ8064"
    CFILES
        src/plat/apq8064/machine/hardware.c
        src/plat/apq8064/machine/l2cache.c
        src/plat/apq8064/machine/io.c
        src/plat/apq8064/machine/timer.c
)
