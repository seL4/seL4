#
# Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
#
# SPDX-License-Identifier: GPL-2.0-only
#

cmake_minimum_required(VERSION 3.7.2)

add_sources(
    DEP "KernelSel4ArchIA32"
    PREFIX src/arch/x86/32
    CFILES
        c_traps.c
        object/objecttype.c
        kernel/thread.c
        kernel/vspace.c
        kernel/vspace_32paging.c
        kernel/elf.c
        model/statedata.c
        machine/registerset.c
        machine/capdl.c
        smp/ipi.c
    ASMFILES machine_asm.S traps.S head.S
)
