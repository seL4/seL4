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
    KernelRiscvExtF RISCV_EXT_F "RISC-V extension for single-precision floating-point"
    DEFAULT OFF
    DEPENDS "KernelArchRiscV"
)

config_option(
    KernelRiscvExtD RISCV_EXT_D "RISC-V extension for double-precision floating-point"
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
    # Actually, RV32 with Sv32 supports a 34-bit (16 GiByte) physical address
    # space. However, we only support a 32-bit address space at the moment when
    # targeting 32-bit architectures.
    set(KernelPhysAddressSpaceBits 32)
elseif(KernelSel4ArchRiscV64)
    # The RISC-V privileged spec v1.12 defines that RV64 always uses a 56-bit
    # physical address space.
    if(KernelPTLevels EQUAL 3)
        # structures.bf limits us to a 39-bit physical address space in Sv39
        # mode with a 39-bit VA space and 3 page table levels
        set(KernelPhysAddressSpaceBits 39)
    else()
        # Sv48: 48-bit VA space with 4 page table levels (unsupported)
        # Sv57: 57-bit VA space with 5 page table levels (unsupported)
        # Sv64: details still unclear
        message(FATAL_ERROR "KernelPTLevels=${KernelPTLevels} is unsupported")
    endif()
else()
    message(FATAL_ERROR "unsuppored RISC-V architecture")
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
