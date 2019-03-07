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

if(KernelPlatformWandQ OR KernelPlatformSabre)
    set(KernelArmCortexA9 ON)
    set(KernelArchArmV7a ON)
    config_set(KernelPlatImx6 PLAT_IMX6 ON)
    config_set(KernelPlatform PLAT "imx6")
    set(KernelArmMach "imx" CACHE INTERNAL "")

    if(KernelPlatformWandQ)
        list(APPEND KernelDTSList "tools/dts/wandq.dts")
        list(APPEND KernelDTSList "src/plat/imx6/overlay-wandq.dts")
    else()
        list(APPEND KernelDTSList "tools/dts/sabre.dts")
        list(APPEND KernelDTSList "src/plat/imx6/overlay-sabre.dts")
    endif()
else()
    config_set(KernelPlatImx6 PLAT_IMX6 OFF)
endif()

add_sources(
    DEP "KernelPlatImx6"
    CFILES
        src/arch/arm/machine/l2c_310.c
        src/arch/arm/machine/gic_pl390.c
)
