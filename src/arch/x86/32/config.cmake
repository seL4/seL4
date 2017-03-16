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
        smp/ipi.c
    ASMFILES
        machine_asm.S
        traps.S
        head.S
)
