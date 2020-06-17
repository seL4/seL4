#
# Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
#
# SPDX-License-Identifier: GPL-2.0-only
#

cmake_minimum_required(VERSION 3.7.2)

add_sources(
    DEP "KernelSel4ArchAarch64"
    PREFIX src/arch/arm/64
    CFILES
        object/objecttype.c
        machine/capdl.c
        machine/registerset.c
        machine/fpu.c
        model/statedata.c
        c_traps.c
        idle.c
        kernel/thread.c
        kernel/vspace.c
    ASMFILES head.S traps.S
)
