#
# Copyright 2020, DornerWorks
# Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
# Copyright 2021, HENSOLDT Cyber
#
# SPDX-License-Identifier: GPL-2.0-only
#

declare_platform(spike KernelPlatformSpike PLAT_SPIKE KernelArchRiscV)

if(KernelPlatformSpike)
    declare_seL4_arch(riscv64 riscv32)
    config_set(KernelRiscVPlatform RISCV_PLAT "spike")
    config_set(KernelPlatformFirstHartID FIRST_HART_ID 0)
    config_set(KernelOpenSBIPlatform OPENSBI_PLATFORM "generic")
    if(KernelSel4ArchRiscV32)
        list(APPEND KernelDTSList "tools/dts/spike32.dts")
    else()
        list(APPEND KernelDTSList "tools/dts/spike.dts")
    endif()
    list(APPEND KernelDTSList "src/plat/spike/overlay-spike.dts")
    declare_default_headers(
        TIMER_FREQUENCY 10000000
        MAX_IRQ 0
        INTERRUPT_CONTROLLER drivers/irq/riscv_plic_dummy.h
    )
else()
    unset(KernelPlatformFirstHartID CACHE)
endif()
