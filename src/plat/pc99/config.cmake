#
# Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
#
# SPDX-License-Identifier: GPL-2.0-only
#

cmake_minimum_required(VERSION 3.7.2)

declare_platform(
    "pc99"
    "x86_64;ia32" # default is first (x86_64)
    NO_DEFAULT_DTS # there is no DTS on x86
    CAMKE_VAR
    "KernelPlatPC99"
    # C_DEFINE defaults to CONFIG_PLAT_PC99
    SOURCES
    "src/plat/pc99/machine/acpi.c"
    "src/plat/pc99/machine/hardware.c"
    "src/plat/pc99/machine/pic.c"
    "src/plat/pc99/machine/ioapic.c"
    "src/plat/pc99/machine/pit.c"
    "src/plat/pc99/machine/io.c"
    "src/plat/pc99/machine/intel-vtd.c"
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

config_string(
    KernelPC99TSCFrequency PC99_TSC_FREQUENCY
    "Provide a static definition of the TSC frequency (in Hz). \
    If this isn't set then the boot code will try and read the frequency from a MSR. \
    If it can't calculate the frequency from a MSR then it will estimate it from running the PIT for about 200ms."
    DEFAULT 0
    DEPENDS "KernelPlatPC99"
    UNQUOTE UNDEF_DISABLED
)
