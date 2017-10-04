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

config_option(KernelIOMMU IOMMU
    "IOMMU support for VT-d enabled chipset"
    DEFAULT ON
    DEPENDS "KernelPlatPC99; NOT KernelVerificationBuild" DEFAULT_DISABLED OFF
)

config_string(KernelMaxRMRREntries MAX_RMRR_ENTRIES
    "Setsthe maximum number of Reserved Memory Region Reporting structures we support \
    recording from the ACPI tables"
    DEFAULT 32
    DEPENDS "KernelIOMMU" DEFAULT_DISABLED 1
    UNQUOTE
)

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

add_bf_source_old("KernelSel4ArchX86_64" "hardware.bf" "include/plat/pc99/plat/64" "plat_mode/machine")
add_bf_source_old("KernelSel4ArchIA32" "hardware.bf" "include/plat/pc99/plat/32" "plat_mode/machine")
