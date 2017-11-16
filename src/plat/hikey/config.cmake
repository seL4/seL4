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

config_string(KernelArmHikeyOutstandingPrefetchers ARM_HIKEY_OUTSTANDING_PREFETCHERS
    "Number of outstanding prefetch allowed \
    Cortex A53 has an L1 Data prefetcher. This config options allows \
    the number of outstanding prefetcher to be set from a number from \
    1 to 7. Note that a setting of 7 maps to 8 and 5 is the reset value."
    DEFAULT 5
    DEPENDS "KernelPlatformHikey;NOT KernelDebugDisablePrefetchers" DEFAULT_DISABLED 0
    UNQUOTE
)
set_property(CACHE KernelArmHikeyOutstandingPrefetchers PROPERTY STRINGS "1;2;3;4;5;6;7")

config_string(KernelArmHikeyPrefetcherStride ARM_HIKEY_PREFETCHER_STRIDE
    "Number of strides before prefetcher is triggered \
    Number of strides before prefetcher is triggered. \
    Allowed values are 2 and 3. 2 is the reset value"
    DEFAULT 2
    DEPENDS "KernelPlatformHikey;NOT KernelDebugDisablePrefetchers" DEFAULT_DISABLED 0
    UNQUOTE
)
set_property(CACHE KernelArmHikeyPrefetcherStride PROPERTY STRINGS "2;3")

config_string(KernelArmHikeyPrefetcherNPFSTRM ARM_HIKEY_PREFETCHER_NPFSTRM
    "Number of indepedent prefetch streams \
    Number of indepedent prefetch streams. Allowed values are 1 to 4.\
    2 is the reset value"
    DEFAULT 2
    DEPENDS "KernelPlatformHikey;NOT KernelDebugDisablePrefetchers" DEFAULT_DISABLED 0
    UNQUOTE
)
set_property(CACHE KernelArmHikeyPrefetcherNPFSTRM PROPERTY STRINGS "1;2;3;4")

config_option(KernelArmHikeyPrefetcherSTBPFDIS ARM_HIKEY_PREFETCHER_STBPFDIS
    "Enable prefetch streams initated by STB access \
    Enable prefetch streams initated by STB access. Enabled is the reset value"
    DEFAULT ON
    DEPENDS "KernelPlatformHikey; NOT KernelDebugDisablePrefetchers" DEFAULT_DISABLED OFF
)

config_option(KernelArmHikeyPrefetcherSTBPFRS ARM_HIKEY_PREFETCHER_STBPFRS
    "Prefetcher to initated on a ReadUnique or ReadShared \
    Sets prefetcher to initated on a ReadUnique (n) or ReadShared (y) \
    ReadUnique is the reset value"
    DEFAULT OFF
    DEPENDS "KernelPlatformHikey;NOT KernelDebugDisablePrefetchers"
)

if(KernelPlatformHikey)
    set(KernelArmCortexA53 ON)
    set(KernelArchArmV8a ON)
    config_set(KernelPlatform PLAT "hikey")
    set(KernelArmMachFeatureModifiers "+crc" CACHE INTERNAL "")
    if(KernelSel4ArchAarch64)
        set(KernelHaveFPU ON)
    endif()
endif()

add_sources(
    DEP "KernelPlatformHikey"
    CFILES
        src/plat/hikey/machine/hardware.c
        src/plat/hikey/machine/io.c
        src/arch/arm/machine/generic_timer.c
)
