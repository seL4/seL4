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

if(KernelPlatformSpike)
    config_set(KernelPlatform PLAT "spike")
endif()

add_sources(
    DEP "KernelPlatformSpike"
    CFILES
        src/plat/spike/machine/fdt.c
        src/plat/spike/machine/hardware.c
)
