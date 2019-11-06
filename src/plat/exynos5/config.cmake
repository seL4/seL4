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

# We introduce a variable to hold this long expression to prevent the
# code styler from line-wrapping the declare_platform() statement.  We
# want to keep that on one line so the `griddle` tool (or humans) can
# easily `grep` a list of supported platforms.  As of 2019-08-07, this
# platform is the only one requiring this workaround.
set(AArch32OrArchArmHyp "KernelSel4ArchAarch32 OR KernelSel4ArchArmHyp")
declare_platform(exynos5 KernelPlatExynos5 PLAT_EXYNOS5 ${AArch32OrArchArmHyp}
    "exynos5250,KernelPlatformExynos5250,PLAT_EXYNOS5250"
    "exynos5410,KernelPlatformExynos5410,PLAT_EXYNOS5410"
    "exynos5422,KernelPlatformExynos5422,PLAT_EXYNOS5422"
)

unset(${AArch32OrArchArmHyp} CACHE)


if(KernelPlatExynos5)
    if("${KernelSel4Arch}" STREQUAL aarch32)
        declare_seL4_arch(aarch32)
    elseif("${KernelSel4Arch}" STREQUAL arm_hyp)
        declare_seL4_arch(arm_hyp)
    else()
        fallback_declare_seL4_arch_default(aarch32)
    endif()
    set(KernelArmCortexA15 ON)
    set(KernelArchArmV7ve ON)
    # v7ve is a superset of v7a, so we enable that as well
    set(KernelArchArmV7a ON)
    config_set(KernelArmMach MACH "exynos")

    if(KernelPlatformExynos5410 OR KernelPlatformExynos5422)
        config_set(KernelPlatExynos54xx PLAT_EXYNOS54XX ON)
    else()
        config_set(KernelPlatExynos54xx PLAT_EXYNOS54XX OFF)
    endif()

    if(NOT KernelPlatformExynos5422)
        set(KernelHardwareDebugAPIUnsupported ON CACHE INTERNAL "")
    endif()

    list(APPEND KernelDTSList "tools/dts/${KernelARMPlatform}.dts")
    list(APPEND KernelDTSList "src/plat/exynos5/overlay-${KernelARMPlatform}.dts")
    declare_default_headers(
        TIMER_FREQUENCY 24000000llu
        MAX_IRQ 232
        NUM_PPI 32
        TIMER drivers/timer/arm_generic.h
        INTERRUPT_CONTROLLER arch/machine/gic_v2.h
        CLK_MAGIC 2863311531llu
        CLK_SHIFT 36u
        KERNEL_WCET 10u
    )
endif()

add_sources(
    DEP "KernelPlatExynos5"
    CFILES src/arch/arm/machine/gic_v2.c src/arch/arm/machine/l2c_nop.c
)
