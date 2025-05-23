#
# Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
#
# SPDX-License-Identifier: GPL-2.0-only
#

declare_platform(hikey KernelPlatformHikey PLAT_HIKEY KernelArchARM)

if(KernelPlatformHikey)
    # For historical reasons, Hikey defaults to aarch32
    declare_seL4_arch(aarch32 aarch64)
    set(KernelArmCortexA53 ON)
    set(KernelArchArmV8a ON)
    config_set(KernelARMPlatform ARM_PLAT hikey)
    set(KernelArmMachFeatureModifiers "+crc" CACHE INTERNAL "")
    list(APPEND KernelDTSList "tools/dts/hikey.dts")
    list(APPEND KernelDTSList "src/plat/hikey/overlay-hikey.dts")
    declare_default_headers(
        TIMER_FREQUENCY 1200000
        MAX_IRQ 159
        TIMER drivers/timer/arm_generic.h
        INTERRUPT_CONTROLLER arch/machine/gic_v2.h
        CLK_MAGIC 458129845llu
        CLK_SHIFT 39u
        KERNEL_WCET 10u
    )
endif()

config_string(
    KernelArmHikeyOutstandingPrefetchers ARM_HIKEY_OUTSTANDING_PREFETCHERS
    "Number of outstanding prefetch allowed \
    Cortex A53 has an L1 Data prefetcher. This config options allows \
    the number of outstanding prefetcher to be set from a number from \
    1 to 7. Note that a setting of 7 maps to 8 and 5 is the reset value."
    DEFAULT 5
    DEPENDS "KernelPlatformHikey;NOT KernelDebugDisablePrefetchers" DEFAULT_DISABLED 0
    UNQUOTE
)
set_property(CACHE KernelArmHikeyOutstandingPrefetchers PROPERTY STRINGS "1;2;3;4;5;6;7")

config_string(
    KernelArmHikeyPrefetcherStride ARM_HIKEY_PREFETCHER_STRIDE
    "Number of strides before prefetcher is triggered \
    Number of strides before prefetcher is triggered. \
    Allowed values are 2 and 3. 2 is the reset value"
    DEFAULT 2
    DEPENDS "KernelPlatformHikey;NOT KernelDebugDisablePrefetchers" DEFAULT_DISABLED 0
    UNQUOTE
)
set_property(CACHE KernelArmHikeyPrefetcherStride PROPERTY STRINGS "2;3")

config_string(
    KernelArmHikeyPrefetcherNPFSTRM ARM_HIKEY_PREFETCHER_NPFSTRM
    "Number of independent prefetch streams \
    Number of independent prefetch streams. Allowed values are 1 to 4.\
    2 is the reset value"
    DEFAULT 2
    DEPENDS "KernelPlatformHikey;NOT KernelDebugDisablePrefetchers" DEFAULT_DISABLED 0
    UNQUOTE
)
set_property(CACHE KernelArmHikeyPrefetcherNPFSTRM PROPERTY STRINGS "1;2;3;4")

config_option(
    KernelArmHikeyPrefetcherSTBPFDIS ARM_HIKEY_PREFETCHER_STBPFDIS
    "Enable prefetch streams initated by STB access \
    Enable prefetch streams initated by STB access. Enabled is the reset value"
    DEFAULT ON
    DEPENDS "KernelPlatformHikey; NOT KernelDebugDisablePrefetchers"
    DEFAULT_DISABLED OFF
)

config_option(
    KernelArmHikeyPrefetcherSTBPFRS ARM_HIKEY_PREFETCHER_STBPFRS
    "Prefetcher to initated on a ReadUnique or ReadShared \
    Sets prefetcher to initated on a ReadUnique (n) or ReadShared (y) \
    ReadUnique is the reset value"
    DEFAULT OFF
    DEPENDS "KernelPlatformHikey;NOT KernelDebugDisablePrefetchers"
)

add_sources(
    DEP "KernelPlatformHikey"
    CFILES src/arch/arm/machine/gic_v2.c src/arch/arm/machine/l2c_nop.c
)
