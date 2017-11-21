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

declare_platform(tx2 KernelPlatformTx2 PLAT_TX2 KernelSel4ArchAarch64)

if(KernelPlatformTx2)
    declare_seL4_arch(aarch64)
    # Note: If we enable the Denver 2 cores, which are 40-bit PA,
    # the 44-bit PA for Cortex-A57 cores would need to be downgraded to 40bit.
    set(KernelArmCortexA57 ON)
    set(KernelArchArmV8a ON)
    config_set(KernelARMPlatform ARM_PLAT tx2)
    config_set(KernelArmMach MACH "nvidia")
    list(APPEND KernelDTSList "tools/dts/tx2.dts")
    list(APPEND KernelDTSList "src/plat/tx2/overlay-tx2.dts")
    declare_default_headers(
        TIMER_FREQUENCY 31250000llu
        MAX_IRQ 383
        INTERRUPT_CONTROLLER arch/machine/gic_v2.h
        NUM_PPI 32
        TIMER drivers/timer/arm_generic.h
        CLK_SHIFT 57u
        CLK_MAGIC 4611686019u
        KERNEL_WCET 10u
    )
endif()

add_sources(
    DEP "KernelPlatformTx2"
    CFILES src/arch/arm/machine/gic_v2.c src/arch/arm/machine/l2c_nop.c
)
