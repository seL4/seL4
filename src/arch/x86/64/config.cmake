#
# Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
#
# SPDX-License-Identifier: GPL-2.0-only
#

cmake_minimum_required(VERSION 3.7.2)

add_sources(
    DEP "KernelSel4ArchX86_64"
    PREFIX src/arch/x86/64
    CFILES
        c_traps.c
        object/objecttype.c
        kernel/thread.c
        kernel/vspace.c
        kernel/elf.c
        model/statedata.c
        model/smp.c
        machine/capdl.c
        machine/registerset.c
        smp/ipi.c
    ASMFILES machine_asm.S traps.S head.S
)
