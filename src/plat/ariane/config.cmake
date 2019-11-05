#
# Copyright 2019, Data61
# Commonwealth Scientific and Industrial Research Organisation (CSIRO)
# ABN 41 687 119 230.
#
# This software may be distributed and modified according to the terms of
# the GNU General Public License version 2. Note that NO WARRANTY is provided.
# See "LICENSE_GPLv2.txt" for details.
#
# @TAG(DATA61_GPL)
#

cmake_minimum_required(VERSION 3.7.2)

declare_platform(ariane KernelPlatformAriane PLAT_ARIANE KernelArchRiscV)

if(KernelPlatformAriane)
    declare_seL4_arch(riscv64)
    config_set(KernelRiscVPlatform RISCV_PLAT "ariane")
    config_set(KernelPlatformFirstHartID FIRST_HART_ID 0)
    list(APPEND KernelDTSList "tools/dts/ariane.dts")
    list(APPEND KernelDTSList "src/plat/ariane/overlay-ariane.dts")
    declare_default_headers(
        TIMER_FREQUENCY 10000000llu PLIC_MAX_NUM_INT 0
        INTERRUPT_CONTROLLER arch/machine/plic.h
    )
else()
    unset(KernelPlatformFirstHartID CACHE)
endif()
