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

declare_platform(pc99 KernelPlatPC99 PLAT_PC99 KernelArchX86)

if(KernelPlatPC99)
    if("${KernelSel4Arch}" STREQUAL ia32)
        declare_seL4_arch(ia32)
    elseif("${KernelSel4Arch}" STREQUAL x86_64)
        declare_seL4_arch(x86_64)
    else()
        fallback_declare_seL4_arch_default(x86_64)
    endif()

endif()

add_sources(
    DEP "KernelPlatPC99"
    PREFIX src/plat/pc99/machine
    CFILES
        acpi.c
        hardware.c
        pic.c
        ioapic.c
        pit.c
        io.c
        intel-vtd.c
)

add_bf_source_old(
    "KernelSel4ArchX86_64"
    "hardware.bf"
    "include/plat/pc99/plat/64"
    "plat_mode/machine"
)
add_bf_source_old(
    "KernelSel4ArchIA32"
    "hardware.bf"
    "include/plat/pc99/plat/32"
    "plat_mode/machine"
)
