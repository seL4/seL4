#
# Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
#
# SPDX-License-Identifier: GPL-2.0-only
#

cmake_minimum_required(VERSION 3.7.2)

declare_platform(
    "rocketchip"
    "riscv64"
    # use default DTS at tools/dts/rocketchip.dts
    CAMKE_VAR
    "KernelPlatformRocketchip"
    # C_DEFINE defaults to CONFIG_PLAT_ROCKETCHIP
)

if(KernelPlatformRocketchip)

    config_set(KernelPlatformFirstHartID FIRST_HART_ID 0)
    config_set(KernelOpenSBIPlatform OPENSBI_PLATFORM "generic")

    declare_default_headers(
        TIMER_FREQUENCY 10000000llu
        PLIC_MAX_NUM_INT 0
        INTERRUPT_CONTROLLER arch/machine/plic.h
    )

endif()
