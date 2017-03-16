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

if(KernelPlatformAM335X)
    set(KernelArmCortexA8 ON)
    set(KernelArchArmV7a ON)
    config_set(KernelPlatform PLAT "am335x")
endif()

add_sources(
    DEP "KernelPlatformAM335X"
    CFILES
        src/plat/am335x/machine/hardware.c
        src/plat/am335x/machine/io.c
        src/plat/am335x/machine/l2cache.c
)
