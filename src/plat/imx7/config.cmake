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

if(KernelPlatformImx7Sabre)
    set(KernelArmCortexA7 ON)
    set(KernelArchArmV7a ON)
    config_set(KernelPlatImx7 PLAT_IMX7 ON)
    config_set(KernelPlatform PLAT "imx7")
    set(KernelArmMach "imx" CACHE INTERNAL "")
    list(APPEND KernelDTSList "tools/dts/imx7sabre.dts")
    list(APPEND KernelDTSList "src/plat/imx7/overlay-imx7sabre.dts")
else()
    config_set(KernelPlatImx7 PLAT_IMX7 OFF)
endif()

add_sources(
    DEP "KernelPlatImx7"
    CFILES 
           src/arch/arm/machine/gic_pl390.c
           src/arch/arm/machine/l2c_nop.c
)
