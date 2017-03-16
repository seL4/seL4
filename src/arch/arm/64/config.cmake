#
# Copyright 2017, Data61
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

add_sources(
    DEP "KernelSel4ArchAarch64"
    PREFIX src/arch/arm/64
    CFILES
        object/objecttype.c
        machine/capdl.c
        machine/hardware.c
        machine/registerset.c
        machine/fpu.c
        model/statedata.c
        c_traps.c
        idle.c
        kernel/thread.c
        kernel/vspace.c
    ASMFILES
        head.S
        traps.S
)
