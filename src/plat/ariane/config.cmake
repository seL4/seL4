#
# Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
#
# SPDX-License-Identifier: GPL-2.0-only
#

cmake_minimum_required(VERSION 3.7.2)

declare_platform(ariane KernelPlatformAriane PLAT_ARIANE KernelArchRiscV)

if(KernelPlatformAriane)
    declare_seL4_arch(riscv64)
    config_set(KernelRiscVPlatform RISCV_PLAT "ariane")
    config_set(KernelPlatformFirstHartID FIRST_HART_ID 0)
    config_set(KernelOpenSBIPlatform OPENSBI_PLATFORM "fpga/ariane")
    list(APPEND KernelDTSList "tools/dts/ariane.dts")
    list(APPEND KernelDTSList "src/plat/ariane/overlay-ariane.dts")
    declare_default_headers(
        TIMER_FREQUENCY 25000000llu PLIC_MAX_NUM_INT 30
        INTERRUPT_CONTROLLER drivers/irq/riscv_plic0.h
    )
else()
    unset(KernelPlatformFirstHartID CACHE)
endif()
