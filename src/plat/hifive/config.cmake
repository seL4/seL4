#
# Copyright 2019, Data61
# Commonwealth Scientific and Industrial Research Organisation (CSIRO)
# ABN 41 687 119 230.
#
# Copyright 2019, DornerWorks
#
# This software may be distributed and modified according to the terms of
# the GNU General Public License version 2. Note that NO WARRANTY is provided.
# See "LICENSE_GPLv2.txt" for details.
#
# @TAG(DATA61_DORNERWORKS_GPL)
#

cmake_minimum_required(VERSION 3.7.2)

declare_platform(hifive KernelPlatformHifive PLAT_HIFIVE KernelSel4ArchRiscV64)

if(KernelPlatformHifive)
    declare_seL4_arch(riscv64)
    config_set(KernelRiscVPlatform RISCV_PLAT "hifive")
    config_set(KernelPlatformFirstHartID FIRST_HART_ID 1)
    list(APPEND KernelDTSList "tools/dts/hifive.dts")
    list(APPEND KernelDTSList "src/plat/hifive/overlay-hifive.dts")
    declare_default_headers(
        TIMER_FREQUENCY 10000000llu PLIC_MAX_NUM_INT 53
        INTERRUPT_CONTROLLER drivers/irq/hifive.h
    )
else()
    unset(KernelPlatformFirstHartID CACHE)
endif()
