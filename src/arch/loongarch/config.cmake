#
# Copyright 2022, tyyteam(Qingtao Liu, Yang Lei, Yang Chen)
# qtliu@mail.ustc.edu.cn, le24@mail.ustc.edu.cn, chenyangcs@mail.ustc.edu.cn
#
# SPDX-License-Identifier: GPL-2.0-only
#

cmake_minimum_required(VERSION 3.7.2)

config_string(
    KernelPTLevels PT_LEVELS "Number of page"
    DEFAULT 3 UNDEF_DISABLED
    UNQUOTE
    DEPENDS "KernelArchLoongarch"
)

config_option(
    KernelLoongarchExtF Loongarch_EXT_F "Loongarch extension for single-precision floating-point"
    DEFAULT OFF
    DEPENDS "KernelArchLoongarch"
)

config_option(
    KernelLoongarchExtD Loongarch_EXT_D "Loongarch extension for double-precision floating-point"
    DEFAULT OFF
    DEPENDS "KernelArchLoongarch"
)
if(KernelWordSize EQUAL 64)
    set(KernelClz64 ON CACHE BOOL "")
    set(KernelCtz64 ON CACHE BOOL "")
endif()

if(KernelPTLevels EQUAL 3)
math(EXPR KernelPaddrUserTop "1 << 48")
elseif(KernelPTLevels EQUAL 4)
    message(FATAL_ERROR "PTLevels 4 not used")
    # math(EXPR KernelPaddrUserTop "1 << 56")
endif()

if(KernelLoongarchExtD)
    # The D extension depends on the base single-precision
    # instruction subset F.
    set(KernelLoongarchExtF ON)
endif()

if(KernelLoongarchExtF)
    set(KernelHaveFPU ON)
endif()

# This is not supported on RISC-V(tyyteam doesn't check it, and just set it ON)
set(KernelHardwareDebugAPIUnsupported ON CACHE INTERNAL "")

add_sources(
    DEP "KernelArchLoongarch"
    PREFIX src/arch/loongarch
    CFILES
        c_traps.c
        idle.c
        api/faults.c
        api/benchmark.c
        kernel/boot.c
        kernel/thread.c
        kernel/vspace.c
        machine/capdl.c
        machine/hardware.c
        machine/registerset.c
        machine/io.c
        machine/fpu.c
        machine/cache.c
        machine/tlb.c
        model/statedata.c
        object/interrupt.c
        object/objecttype.c
        object/tcb.c
        smp/ipi.c
    ASMFILES head.S traps.S tlbex.S tlbrefill.S
)

add_bf_source_old("KernelArchLoongarch" "structures.bf" "include/arch/loongarch" "arch/object")
