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

declare_platform(imx6 KernelPlatImx6 PLAT_IMX6 KernelSel4ArchAarch32
    "sabre,KernelPlatformSabre,PLAT_SABRE"
    "wandq,KernelPlatformWandQ,PLAT_WANDQ"
)

if(KernelPlatImx6)
    declare_seL4_arch(aarch32)
    set(KernelArmCortexA9 ON)
    set(KernelArchArmV7a ON)
    set(KernelArmMach "imx" CACHE INTERNAL "")
    list(APPEND KernelDTSList "tools/dts/${KernelARMPlatform}.dts")
    list(APPEND KernelDTSList "src/plat/imx6/overlay-${KernelARMPlatform}.dts")

    if(KernelIsMCS)
        list(APPEND KernelDTSList "src/plat/imx6/mcs-overlay-imx6.dts")
        set(timer_file drivers/timer/arm_global.h)
        set(timer_freq 498000000llu)
    else()
        set(timer_file drivers/timer/arm_priv.h)
        set(timer_freq 400000000llu)
    endif()

    declare_default_headers(
        TIMER_FREQUENCY ${timer_freq}
        MAX_IRQ 159
        INTERRUPT_CONTROLLER arch/machine/gic_v2.h
        NUM_PPI 32
        TIMER ${timer_file}
        CLK_SHIFT 41llu
        CLK_MAGIC 4415709349llu
        KERNEL_WCET 10llu
        TIMER_PRECISION 2u
    )
endif()

add_sources(
    DEP "KernelPlatImx6"
    CFILES src/arch/arm/machine/l2c_310.c src/arch/arm/machine/gic_v2.c
)
