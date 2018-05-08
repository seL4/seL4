#
# Copyright 2018, Data61
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

config_choice(KernelRiscVSel4Arch RISCV_SEL4_ARCH "Architecture mode for building the kernel"
    "riscv32;KernelSel4ArchRiscV32;ARCH_RISCV32;KernelArchRiscV"
    "riscv64;KernelSel4ArchRiscV64;ARCH_RISCV64;KernelArchRiscV"
)

config_choice(KernelRiscVPlatform RISCV_PLAT "Select the platform for the architecture"
    "spike;KernelPlatformSpike;PLAT_SPIKE;KernelArchRiscV"
)

if(KernelArchRiscV)
    config_set(KernelSel4Arch SEL4_ARCH "${KernelRiscVSel4Arch}")
endif()

if(KernelSel4ArchRiscV32)
    set_kernel_32()
elseif(KernelSel4ArchRiscV64)
    set_kernel_64()
endif()

# Include all the platforms.
include(src/plat/spike/config.cmake)


config_string(KernelPTLevels PT_LEVELS "Number of page \
    table levels for RISC-V depends on the mode. For example there are: \
    2, 3 and 4 levels on Sv32, Sv39, Sv48 RISC-V paging modes respectively."
    DEFAULT 3
    UNDEF_DISABLED
    UNQUOTE
    DEPENDS "KernelArchRiscV"
)

if (KernelSel4ArchRiscV32)
    set(KernelPTLevels 2 CACHE STRING "" FORCE)
endif()

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
        model/statedata.c
        object/interrupt.c
        object/objecttype.c
        object/tcb.c
    ASMFILES
        halt.S
        head.S
        traps.S
)

add_bf_source_old("KernelArchRiscV" "structures.bf" "include/arch/riscv" "arch/object")
add_bf_source_old("KernelArchRiscV" "hardware.bf" "include/plat/${KernelPlatform}" "plat/machine")

