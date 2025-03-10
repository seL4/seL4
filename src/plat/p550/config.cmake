#
# Copyright 2025, UNSW
#
# SPDX-License-Identifier: GPL-2.0-only
#

cmake_minimum_required(VERSION 3.7.2)

declare_platform(p550 KernelPlatformP550 PLAT_P550 KernelArchRiscV)

if(KernelPlatformP550)
    declare_seL4_arch(riscv64)
    config_set(KernelRiscVPlatform RISCV_PLAT "p550")
    config_set(KernelPlatformFirstHartID FIRST_HART_ID 0)
    config_set(KernelOpenSBIPlatform OPENSBI_PLATFORM "generic")
    # Note that by default the kernel is configured for the 16GB model.
    list(APPEND KernelDTSList "tools/dts/p550.dts")
    list(APPEND KernelDTSList "${CMAKE_CURRENT_LIST_DIR}/overlay-p550.dts")
    # The value for TIMER_FREQUENCY is from the "timebase-frequency" field on
    # the "cpus" node in the P550 device tree.
    # The value for MAX_IRQ comes from the DTS "interrupt-controller" node which says
    # "riscv,ndev = <0x208>".
    declare_default_headers(
        TIMER_FREQUENCY 1000000
        MAX_IRQ 520
        INTERRUPT_CONTROLLER drivers/irq/riscv_plic0.h
    )
else()
    unset(KernelPlatformFirstHartID CACHE)
endif()
