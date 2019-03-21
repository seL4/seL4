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

if(KernelPlatformOMAP3)
    set(KernelArmCortexA8 ON)
    set(KernelArchArmV7a ON)
    config_set(KernelPlatform PLAT "omap3")
    config_set(KernelArmMach MACH "omap")
    list(APPEND KernelDTSList "tools/dts/omap3.dts")
    list(APPEND KernelDTSList "src/plat/omap3/overlay-omap3.dts")
endif()

add_sources(
    DEP "KernelPlatformOMAP3"
    CFILES src/plat/omap3/machine/hardware.c src/plat/omap3/machine/l2cache.c
)
