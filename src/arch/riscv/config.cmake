#
# Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
#
# SPDX-License-Identifier: GPL-2.0-only
#

cmake_minimum_required(VERSION 3.16.0)

config_string(
    KernelPTLevels PT_LEVELS "Number of page \
    table levels for RISC-V depends on the mode. For example there are: \
    2, 3 and 4 levels on Sv32, Sv39, Sv48 RISC-V paging modes respectively."
    DEFAULT 3
    UNDEF_DISABLED UNQUOTE
    DEPENDS "KernelArchRiscV"
)

set(_KernelRiscvExtD ON)
set(_KernelRiscvExtF ON)
if(LLVM_TOOLCHAIN AND KernelSel4ArchRiscV32)
    # Versions of clang we support can't compile for D double width floating
    # point. But we've found that having F but not D still leads to errors with
    # code that assumes if any floating point is enabled, both F and D are enabled.
    set(_KernelRiscvExtD OFF)
    set(_KernelRiscvExtF OFF)
endif()

config_option(
    KernelRiscvExtF RISCV_EXT_F "RISC-V extension for single-precision floating-point"
    DEFAULT ${_KernelRiscvExtF}
    DEPENDS "KernelArchRiscV"
)

config_option(
    KernelRiscvExtD RISCV_EXT_D "RISC-V extension for double-precision floating-point"
    DEFAULT ${_KernelRiscvExtD}
    DEPENDS "KernelArchRiscV"
)

config_option(
    KernelRiscvUseClintMtime RISCV_USE_CLINT_MTIME "When reading the timestamp \
    from the hardware, directly access the CLINT timer register (mtime) instead \
    of using the rdtime instruction. This is a performance optimization, but \
    only for platforms where executing the rdtime instruction results in a \
    trap into M-mode software which then accesses the CLINT timer register. \
    Note that this option requires S-mode access to the CLINT."
    DEFAULT OFF
    DEPENDS "KernelArchRiscV"
)

# Until RISC-V has instructions to count leading/trailing zeros, we provide
# library implementations. Platforms that implement the bit manipulation
# extension can override these settings to remove the library functions from
# the image.
# In the verified configurations, we additionally define KernelClzNoBuiltin and
# KernelCtzNoBuiltin to expose the library implementations to verification.
# However, since the NoBuiltin options force the use of the library functions
# even when the platform has sutiable inline assembly, we do not make these the
# default.
if(KernelWordSize EQUAL 32)
    set(KernelClz32 ON CACHE BOOL "")
    set(KernelCtz32 ON CACHE BOOL "")
    if(KernelIsMCS)
        # Used for long division in timer calculations.
        set(KernelClz64 ON CACHE BOOL "")
    endif()
elseif(KernelWordSize EQUAL 64)
    set(KernelClz64 ON CACHE BOOL "")
    set(KernelCtz64 ON CACHE BOOL "")
endif()

if(KernelSel4ArchRiscV32)
    set(KernelPTLevels 2 CACHE STRING "" FORCE)
endif()
if(KernelPTLevels EQUAL 2)
    if(KernelSel4ArchRiscV32)
        # seL4 on RISCV32 uses 32-bit ints for addresses,
        # so limit the maximum paddr to 32-bits.
        math(EXPR KernelPaddrUserTop "(1 << 32) - 1")
    else()
        math(EXPR KernelPaddrUserTop "1 << 34")
    endif()
elseif(KernelPTLevels EQUAL 3)
    # RISC-V technically supports 56-bit paddrs,
    # but structures.bf limits us to using 39 of those bits.
    math(EXPR KernelPaddrUserTop "1 << 39")
elseif(KernelPTLevels EQUAL 4)
    math(EXPR KernelPaddrUserTop "1 << 56")
endif()

if(KernelRiscvExtD)
    # The D extension depends on the base single-precision
    # instruction subset F.
    set(KernelRiscvExtF ON)
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
        machine/capdl.c
        machine/hardware.c
        machine/registerset.c
        machine/io.c
        machine/fpu.c
        model/statedata.c
        object/interrupt.c
        object/objecttype.c
        object/tcb.c
        smp/ipi.c
    ASMFILES head.S traps.S
)

add_bf_source_old("KernelArchRiscV" "structures.bf" "include/arch/riscv" "arch/object")
