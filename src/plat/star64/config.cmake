#
# Copyright 2023, Ivan Velickovic
#
# SPDX-License-Identifier: GPL-2.0-only
#

declare_platform(star64 KernelPlatformStar64 PLAT_STAR64 KernelArchRiscV)

if(KernelPlatformStar64)
    declare_seL4_arch(riscv64)
    config_set(KernelRiscVPlatform RISCV_PLAT "star64")
    # The JH7110 SoC contains the SiFive U74-MC core complex. This has four U74
    # cores and one S7 core (which has a hart ID of 0). The first U74 core has
    # a hart ID of 1.
    config_set(KernelPlatformFirstHartID FIRST_HART_ID 1)
    config_set(KernelOpenSBIPlatform OPENSBI_PLATFORM "generic")
    # Note that by default the kernel is configured for the 4GB Star64 model.
    list(APPEND KernelDTSList "tools/dts/star64.dts")
    list(APPEND KernelDTSList "${CMAKE_CURRENT_LIST_DIR}/overlay-star64.dts")
    # The value for TIMER_FREQUENCY is from the "timebase-frequency" field on
    # the "cpus" node in the Star64 device tree.
    # The value for MAX_IRQ comes from the DTS "plic" node which says
    # "riscv,ndev = <0x88>".
    declare_default_headers(
        TIMER_FREQUENCY 4000000
        MAX_IRQ 136
        INTERRUPT_CONTROLLER drivers/irq/riscv_plic0.h
    )
else()
    unset(KernelPlatformFirstHartID CACHE)
endif()
