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

declare_platform(zynq7000 KernelPlatformZynq7000 PLAT_ZYNQ7000 KernelSel4ArchAarch32)

if(KernelPlatformZynq7000)
    declare_seL4_arch(aarch32)
    set(KernelArmCortexA9 ON)
    set(KernelArchArmV7a ON)
    config_set(KernelArmMach MACH "zynq")
    list(APPEND KernelDTSList "tools/dts/zynq7000.dts")
    if(KernelIsMCS)
        list(APPEND KernelDTSList "src/plat/zynq7000/mcs-overlay-zynq7000.dts")
        set(timer_file drivers/timer/arm_global.h)
        set(timer_freq 433000000llu)
    else()
        list(APPEND KernelDTSList "src/plat/zynq7000/overlay-zynq7000.dts")
        set(timer_file drivers/timer/arm_priv.h)
        set(timer_freq 400000000llu)
    endif()
    declare_default_headers(
        TIMER_FREQUENCY ${timer_freq}
        MAX_IRQ 92
        NUM_PPI 32
        INTERRUPT_CONTROLLER arch/machine/gic_v2.h
        TIMER ${timer_file}
        CLK_SHIFT 36llu
        CLK_MAGIC 158705489llu
        KERNEL_WCET 10llu
    )
endif()

add_sources(
    DEP "KernelPlatformZynq7000"
    CFILES src/arch/arm/machine/l2c_310.c src/arch/arm/machine/gic_v2.c
)
