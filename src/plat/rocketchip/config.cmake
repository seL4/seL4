#
# Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
# Copyright 2021, HENSOLDT Cyber
#
# SPDX-License-Identifier: GPL-2.0-only
#

cmake_minimum_required(VERSION 3.7.2)

declare_platform(
    "rocketchip"
    "riscv64"
    # use default DTS at tools/dts/<board-name>.dts
    CAMKE_VAR
    "KernelPlatformRocketchip"
    # C_DEFINE defaults to CONFIG_PLAT_ROCKETCHIP
)

if(KernelPlatformRocketchip)

    config_set(KernelPlatformFirstHartID FIRST_HART_ID 0)
    config_set(KernelOpenSBIPlatform OPENSBI_PLATFORM "generic")
    # This is an experimental platform that supports accessing peripherals, but
    # the status of support for external interrupts via a PLIC is unclear and
    # may differ depending on the version that is synthesized. Declaring no
    # interrupts and using the dummy PLIC driver seems the best option for now
    # to avoid confusion or even crashes.
    declare_default_headers(
        TIMER_FREQUENCY 10000000
        PLIC_MAX_NUM_INT 0
        INTERRUPT_CONTROLLER drivers/irq/riscv_plic_dummy.h
    )

endif()
