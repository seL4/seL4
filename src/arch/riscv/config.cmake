#
# Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
#
# SPDX-License-Identifier: GPL-2.0-only
#

cmake_minimum_required(VERSION 3.7.2)

config_string(
    KernelPTLevels PT_LEVELS "Number of page \
    table levels for RISC-V depends on the mode. For example there are: \
    2, 3 and 4 levels on Sv32, Sv39, Sv48 RISC-V paging modes respectively."
    DEFAULT 3 UNDEF_DISABLED
    UNQUOTE
    DEPENDS "KernelArchRiscV"
)

config_option(
    KernelRiscvExtF RISCV_EXT_F "RISCV extension for single-preciison floating-point"
    DEFAULT OFF
    DEPENDS "KernelArchRiscV"
)

config_option(
    KernelRiscvExtD RISCV_EXT_D "RISCV extension for double-precision floating-point"
    DEFAULT OFF
    DEPENDS "KernelArchRiscV"
)

if(KernelSel4ArchRiscV32)
    set(KernelPTLevels 2 CACHE STRING "" FORCE)
endif()
if(KernelPTLevels EQUAL 2)
    if(KernelSel4ArchRiscV32)
        # seL4 on RISCV32 uses 32-bit ints for addresses,
        # so limit the maximum paddr to 32-bits.
        math(EXPR KernelPaddrUserTop "(1 << 32) - 1")
    else()
        math(EXPR KernelPaddrUserTop "(1 << 34) - 1")
    endif()
elseif(KernelPTLevels EQUAL 3)
    # RISC-V technically supports 56-bit paddrs,
    # but structures.bf limits us to using 39 of those bits.
    math(EXPR KernelPaddrUserTop "(1 << 39) - 1")
elseif(KernelPTLevels EQUAL 4)
    math(EXPR KernelPaddrUserTop "(1 << 56) - 1")
endif()

if(KernelRiscvExtD)
    set(KernelRiscvExtF ON)
    set(KernelHaveFPU ON)
endif()

if(KernelRiscvExtF)
    set(KernelHaveFPU ON)
endif()

# This is not supported on RISC-V
set(KernelHardwareDebugAPIUnsupported ON CACHE INTERNAL "")

add_sources(
    DEP "KernelArchRiscV"
    PREFIX src/arch/riscv
    CFILES
        c_traps.c
        idle.c
        api/faults.c
        api/benchmark.c
        kernel/boot.c
        kernel/thread.c
        kernel/vspace.c
        machine/hardware.c
        machine/registerset.c
        machine/io.c
        machine/fpu.c
        model/statedata.c
        object/interrupt.c
        object/objecttype.c
        object/tcb.c
        smp/ipi.c
    ASMFILES halt.S head.S traps.S
)

add_sources(DEP "KernelDebugBuild;KernelSel4ArchRiscV32" CFILES src/arch/riscv/machine/capdl.c)

add_bf_source_old("KernelArchRiscV" "structures.bf" "include/arch/riscv" "arch/object")
