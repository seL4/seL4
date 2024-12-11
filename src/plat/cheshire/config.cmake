#
# Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
# Copyright 2021, HENSOLDT Cyber
# Copyright 2024, UNSW
#
# SPDX-License-Identifier: GPL-2.0-only
#

cmake_minimum_required(VERSION 3.7.2)

declare_platform(cheshire KernelPlatformCheshire PLAT_CHESHIRE KernelArchRiscV)

if(KernelPlatformCheshire)
    declare_seL4_arch(riscv64)
    config_set(KernelRiscVPlatform RISCV_PLAT "cheshire")
    config_set(KernelPlatformFirstHartID FIRST_HART_ID 0)
    config_set(KernelOpenSBIPlatform OPENSBI_PLATFORM "fpga/cheshire")
    set(OPENSBI_PLAT_XLEN "64")
    set(OPENSBI_PLAT_ISA "rv64imafdc_zicsr_zifencei")
    list(APPEND KernelDTSList "tools/dts/cheshire.dts")
    list(APPEND KernelDTSList "src/plat/cheshire/overlay-cheshire.dts")
    # This is an experimental platform that supports accessing peripherals, but
    # the status of support for external interrupts via a PLIC is unclear and
    # may differ depending on the version that is synthesized. Declaring no
    # interrupts and using the dummy PLIC driver seems the best option for now
    # to avoid confusion or even crashes.
    declare_default_headers(
        TIMER_FREQUENCY 1000000 PLIC_MAX_NUM_INT 30
        MAX_IRQ 30
        INTERRUPT_CONTROLLER drivers/irq/riscv_plic0.h
    )
else()
    unset(KernelPlatformFirstHartID CACHE)
endif()
