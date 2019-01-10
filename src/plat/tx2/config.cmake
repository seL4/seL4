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

if(KernelPlatformTx2)
    set(KernelArmCortexA57 ON)
    set(KernelArchArmV8a ON)
    config_set(KernelPlatform PLAT "tx2")
    config_set(KernelArmMach MACH "nvidia")
    set(KernelHaveFPU ON)
    # Note that the 44-bit PA is only for Cortex-A57 cores.
    # If we enable the Denver 2 cores, which support 40-bit PA,
    # KernelArmPASizeBits40 should be enabled instead.
    set(KernelArmPASizeBits44 ON)
    list(APPEND KernelDTSList "tools/dts/tx2.dts")
    list(APPEND KernelDTSList "src/plat/tx2/overlay-tx2.dts")
endif()

add_sources(
    DEP "KernelPlatformTx2"
    CFILES
        src/plat/tx2/machine/hardware.c
        src/plat/tx2/machine/io.c
        src/plat/tx2/machine/l2cache.c
        src/arch/arm/machine/generic_timer.c
)
