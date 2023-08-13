#
# Copyright 2020, DornerWorks
# Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
#
# SPDX-License-Identifier: GPL-2.0-only
#

cmake_minimum_required(VERSION 3.7.2)

declare_platform(hifive KernelPlatformHifive PLAT_HIFIVE KernelSel4ArchRiscV64)

if(KernelPlatformHifive)
    declare_seL4_arch(riscv64)
    config_set(KernelRiscVPlatform RISCV_PLAT "hifive")
    config_set(KernelPlatformFirstHartID FIRST_HART_ID 1)
    config_set(KernelOpenSBIPlatform OPENSBI_PLATFORM "generic")
    set(KernelRiscvUseClintMtime ON)
    list(APPEND KernelDTSList "tools/dts/hifive.dts")
    list(APPEND KernelDTSList "src/plat/hifive/overlay-hifive.dts")
    declare_default_headers(
        TIMER_FREQUENCY 1000000 PLIC_MAX_NUM_INT 53
        INTERRUPT_CONTROLLER drivers/irq/riscv_plic0.h
    )
else()
    unset(KernelPlatformFirstHartID CACHE)
endif()
