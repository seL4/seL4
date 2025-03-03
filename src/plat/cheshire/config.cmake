#
# Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
# Copyright 2021, HENSOLDT Cyber
# Copyright 2024, UNSW
#
# SPDX-License-Identifier: GPL-2.0-only
#

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
    declare_default_headers(
        TIMER_FREQUENCY 1000000
        MAX_IRQ 51
        INTERRUPT_CONTROLLER drivers/irq/riscv_plic0.h
    )
else()
    unset(KernelPlatformFirstHartID CACHE)
endif()
