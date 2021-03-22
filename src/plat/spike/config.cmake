#
# Copyright 2020, DornerWorks
# Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
# Copyright 2021, HENSOLDT Cyber
#
# SPDX-License-Identifier: GPL-2.0-only
#

cmake_minimum_required(VERSION 3.7.2)

declare_platform(
    "spike"
    "riscv64;riscv32" # default is first (riscv64)
    NO_DEFAULT_DTS # can't use tools/dts/<board-name>.dts in RV32 mode
    CAMKE_VAR
    "KernelPlatformSpike"
    # C_DEFINE defaults to CONFIG_PLAT_SPIKE
)

if(KernelPlatformSpike)

    config_set(KernelPlatformFirstHartID FIRST_HART_ID 0)
    config_set(KernelOpenSBIPlatform OPENSBI_PLATFORM "generic")

    if(KernelSel4ArchRiscV64)
        list(APPEND KernelDTSList "tools/dts/spike.dts")
    elseif(KernelSel4ArchRiscV32)
        list(APPEND KernelDTSList "tools/dts/spike32.dts")
    else()
        message(FATAL_ERROR "invalid architecture")
    endif()

    declare_default_headers(
        TIMER_FREQUENCY 10000000
        PLIC_MAX_NUM_INT 0
        INTERRUPT_CONTROLLER drivers/irq/riscv_plic_dummy.h
    )

endif()
