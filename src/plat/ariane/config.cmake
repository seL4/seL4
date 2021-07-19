#
# Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
#
# SPDX-License-Identifier: GPL-2.0-only
#

cmake_minimum_required(VERSION 3.7.2)

declare_platform(
    "ariane"
    "riscv64"
    # use default DTS at tools/dts/ariane.dts
    CAMKE_VAR
    "KernelPlatformAriane"
    # C_DEFINE defaults to CONFIG_PLAT_ARIANE
)

if(KernelPlatformAriane)

    config_set(KernelPlatformFirstHartID FIRST_HART_ID 0)
    config_set(KernelOpenSBIPlatform OPENSBI_PLATFORM "fpga/ariane")
    list(APPEND KernelDTSList "${CMAKE_CURRENT_LIST_DIR}/overlay-ariane.dts")

    declare_default_headers(
        TIMER_FREQUENCY 25000000llu
        PLIC_MAX_NUM_INT 30
        INTERRUPT_CONTROLLER drivers/irq/riscv_plic0.h
    )

endif()
