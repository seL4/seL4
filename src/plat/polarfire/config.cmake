#
# Copyright 2020, DornerWorks
#
# SPDX-License-Identifier: GPL-2.0-only
#

cmake_minimum_required(VERSION 3.7.2)

declare_platform(polarfire KernelPlatformPolarfire PLAT_POLARFIRE KernelSel4ArchRiscV64)

if(KernelPlatformPolarfire)
    declare_seL4_arch(riscv64)
    config_set(KernelRiscVPlatform RISCV_PLAT "polarfire")
    config_set(KernelOpenSBIPlatform OPENSBI_PLATFORM "generic")
    config_set(KernelPlatformFirstHartID FIRST_HART_ID 1)
    list(APPEND KernelDTSList "tools/dts/mpfs_icicle.dts")
    list(APPEND KernelDTSList "src/plat/polarfire/overlay-polarfire.dts")
    declare_default_headers(
        TIMER_FREQUENCY 10000000 PLIC_MAX_NUM_INT 186
        INTERRUPT_CONTROLLER drivers/irq/riscv_plic0.h
    )
else()
    unset(KernelPlatformFirstHartID CACHE)
endif()
