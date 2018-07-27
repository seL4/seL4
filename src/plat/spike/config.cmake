#
# Copyright 2018, Data61
# Commonwealth Scientific and Industrial Research Organisation (CSIRO)
# ABN 41 687 119 230.
#
# Copyright 2018, DornerWorks
#
# This software may be distributed and modified according to the terms of
# the GNU General Public License version 2. Note that NO WARRANTY is provided.
# See "LICENSE_GPLv2.txt" for details.
#
# @TAG(DATA61_DORNERWORKS_GPL)
#

cmake_minimum_required(VERSION 3.7.2)

if(KernelPlatformSpike)
    config_set(KernelPlatform PLAT "spike")
endif()

config_option(KernelPlatformSpikeRocketChip BUILD_ROCKET_CHIP_ZEDBOARD "Build application to \
    run on the Rocket-Chip for the zedboard."
    DEFAULT OFF
    DEPENDS "KernelSel4ArchRiscV64;KernelPlatformSpike"
)

config_string(KernelPlatformSpikeClockFrequency SPIKE_CLOCK_FREQ
    "Frequency of Clock used for Scheduler"
    DEFAULT 10000000
    UNQUOTE
)

add_sources(
    DEP "KernelPlatformSpike"
    CFILES
        src/plat/spike/machine/fdt.c
        src/plat/spike/machine/hardware.c
)
