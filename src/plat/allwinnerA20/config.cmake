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

if(KernelPlatformAllwinnerA20)
    set(KernelArmCortexA7 ON)
    set(KernelArchArmV7a ON)
    config_set(KernelPlatAllwinnerA20 PLAT_ALLWINNERA20 ON)
    config_set(KernelPlatform PLAT "allwinnerA20")
    set(KernelArmMach "allwinner" CACHE INTERNAL "")
    list(APPEND KernelDTSList "tools/dts/allwinnera20.dts")
else()
    config_set(KernelPlatAllwinnerA20 PLAT_ALLWINNERA20 OFF)
endif()

add_sources(
    DEP "KernelPlatAllwinnerA20"
    CFILES src/plat/allwinnerA20/machine/io.c
           src/plat/allwinnerA20/machine/hardware.c
           src/plat/allwinnerA20/machine/l2cache.c
           src/arch/arm/machine/gic_pl390.c
)
