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

declare_platform(exynos4 KernelPlatformExynos4 PLAT_EXYNOS4 KernelSel4ArchAarch32)

if(KernelPlatformExynos4)
    declare_seL4_arch(aarch32)
    set(KernelArmCortexA9 ON)
    set(KernelArchArmV7a ON)
    config_set(KernelArmMach MACH "exynos")
    list(APPEND KernelDTSList "tools/dts/exynos4.dts")
    list(APPEND KernelDTSList "src/plat/exynos4/overlay-exynos4.dts")
    declare_default_headers(
        TIMER_FREQUENCY 24000000llu
        MAX_IRQ 159
        NUM_PPI 32
        TIMER drivers/timer/exynos4412-mct.h
        INTERRUPT_CONTROLLER arch/machine/gic_v2.h
        KERNEL_WCET 10u
        CLK_MAGIC 2863311531llu
        CLK_SHIFT 36u
        TIMER_PRECISION 0u
    )
endif()

add_sources(
    DEP "KernelPlatformExynos4"
    CFILES src/arch/arm/machine/l2c_310.c src/arch/arm/machine/gic_v2.c
)
