#
# Copyright 2022, tyyteam(Qingtao Liu, Yang Lei, Yang Chen)
# qtliu@mail.ustc.edu.cn, le24@mail.ustc.edu.cn, chenyangcs@mail.ustc.edu.cn
# SPDX-License-Identifier: GPL-2.0-only
#

cmake_minimum_required(VERSION 3.7.2)

declare_platform(3A5000 KernelPlatform3A5000 PLAT_3A5000 KernelArchLoongarch)
if(KernelPlatform3A5000)
    if("${KernelSel4Arch}" STREQUAL loongarch64)
        declare_seL4_arch(loongarch64)
    else()
        fallback_declare_seL4_arch_default(loongarch64)
    endif()
    # MCS is not supported on 3A5000 temporarly. MCS requires a timer driver that
    # implements the tickless programming requirements, but tyyteam doesn't check it yet.
    # so we just set it OFF
    set(KernelPlatformSupportsMCS OFF)
    config_set(KernelLoongarchPlatform Loongarch_PLAT "3A5000")
    config_set(KernelPlatformFirstHartID FIRST_HART_ID 0)
    if(KernelSel4ArchLoongarch64)
        list(APPEND KernelDTSList "tools/dts/3A5000.dts")
    endif()

    declare_default_headers(
        TIMER_FREQUENCY 100000000 HW_MAX_NUM_INT 8
        INTERRUPT_CONTROLLER drivers/irq/loongarch_extio_dummy.h
    )
    
else()
    unset(KernelPlatformFirstHartID CACHE)
endif()


