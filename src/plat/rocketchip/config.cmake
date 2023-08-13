#
# Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
# Copyright 2021, HENSOLDT Cyber
# Copyright 2023, DornerWorks
#
# SPDX-License-Identifier: GPL-2.0-only
#

cmake_minimum_required(VERSION 3.7.2)

declare_platform(rocketchip KernelPlatformRocketchip PLAT_ROCKETCHIP KernelArchRiscV)

set(c_configs PLAT_ROCKETCHIP_BASE PLAT_ROCKETCHIP_ZCU102)
set(cmake_configs KernelPlatformRocketchipBase KernelPlatformRocketchipZCU102)

set(plat_lists rocketchip-base rocketchip-zcu102)
foreach(config IN LISTS cmake_configs)
    unset(${config} CACHE)
endforeach()

if(KernelPlatformRocketchip)
    declare_seL4_arch(riscv64)

    check_platform_and_fallback_to_default(KernelRiscVPlatform "rocketchip-base")
    list(FIND plat_lists ${KernelRiscVPlatform} index)
    if("${index}" STREQUAL "-1")
        message(FATAL_ERROR "Which rocketchip platform not specified")
    endif()
    list(GET c_configs ${index} c_config)
    list(GET cmake_configs ${index} cmake_config)
    config_set(KernelRiscVPlatform RISCV_PLAT ${KernelRiscVPlatform})
    config_set(${cmake_config} ${c_config} ON)

    config_set(KernelPlatformFirstHartID FIRST_HART_ID 0)
    set(KernelRiscvUseClintMtime ON)
    list(APPEND KernelDTSList "tools/dts/rocketchip.dts")
    # The Rocketchip-ZCU102 is a softcore instantiation that runs on the ZCU102's
    # FPGA fabric. Information on generating and running seL4 on the platform can
    # be found at https://docs.sel4.systems/Hardware/
    if(KernelPlatformRocketchipZCU102)
        # The rocket-fpga-zcu104 platform can be found at the following git repo:
        # https://github.com/bao-project/opensbi/tree/bao/rocket
        #
        # In order for this to function, please ensure the bao-project/opensbi
        # repo is added as a remote to the tools/opensbi project in the seL4 codebase
        config_set(KernelOpenSBIPlatform OPENSBI_PLATFORM "rocket-fpga-zcu104")
        list(APPEND KernelDTSList "src/plat/rocketchip/overlay-rocketchip-zcu102.dts")
        # The zcu102 instantiation supports the PLIC and external interrupts
        declare_default_headers(
            TIMER_FREQUENCY 10000000 PLIC_MAX_NUM_INT 128
            INTERRUPT_CONTROLLER drivers/irq/riscv_plic0.h
        )
    else()
        list(APPEND KernelDTSList "src/plat/rocketchip/overlay-rocketchip-base.dts")
        config_set(KernelOpenSBIPlatform OPENSBI_PLATFORM "generic")
        # This is an experimental platform that supports accessing peripherals, but
        # the status of support for external interrupts via a PLIC is unclear and
        # may differ depending on the version that is synthesized. Declaring no
        # interrupts and using the dummy PLIC driver seems the best option for now
        # to avoid confusion or even crashes.
        declare_default_headers(
            TIMER_FREQUENCY 10000000 PLIC_MAX_NUM_INT 0
            INTERRUPT_CONTROLLER drivers/irq/riscv_plic_dummy.h
        )
    endif()
else()
    unset(KernelPlatformFirstHartID CACHE)
endif()
