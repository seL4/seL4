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

config_choice(
    KernelSpikeInstance
    RISCV_SPIKE_INSTANCE
    "Select the instance for Spike to run on"
    "qemu;KernelPlatformSpikeQemu;BUILD_SPIKE_QEMU;KernelArchRiscV"
    "rocket-chip-zedboard;KernelPlatformSpikeRocketChip;BUILD_ROCKET_CHIP_ZEDBOARD;KernelSel4ArchRiscV64"
    "hi-five-unleashed;KernelPlatformSpikeSiFiveFreedom;BUILD_HI_FIVE_UNLEASHED;KernelSel4ArchRiscV64"
)

config_string(
    KernelPlatformSpikeClockFrequency SPIKE_CLOCK_FREQ "Frequency of Clock used for Scheduler"
    DEFAULT 10000000
    UNQUOTE
)

set(DefaultFirstHartID 0)
# Include all of the different instances of the Spike platform
include(src/plat/spike/instance/qemu/config.cmake)
include(src/plat/spike/instance/rocket-chip/config.cmake)
include(src/plat/spike/instance/freedom/config.cmake)

config_string(
    KernelPlatformSpikeFirstHartID FIRST_HART_ID "HART ID of the first kernel HART "
    DEFAULT ${DefaultFirstHartID}
    UNQUOTE
)

add_sources(DEP "KernelPlatformSpike" CFILES src/plat/spike/machine/hardware.c)
