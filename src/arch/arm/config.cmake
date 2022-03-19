#
# Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
#
# SPDX-License-Identifier: GPL-2.0-only
#

cmake_minimum_required(VERSION 3.7.2)

if(KernelArchARM)
    set_property(TARGET kernel_config_target APPEND PROPERTY TOPLEVELTYPES pde_C)
endif()

set(KernelArmPASizeBits40 OFF)
set(KernelArmPASizeBits44 OFF)
if(KernelArmCortexA35)
    set(KernelArmICacheVIPT ON)
    set(KernelArmPASizeBits40 ON)
    math(EXPR KernelPaddrUserTop "(1 << 40)")
elseif(KernelArmCortexA53)
    set(KernelArmICacheVIPT ON)
    set(KernelArmPASizeBits40 ON)
    math(EXPR KernelPaddrUserTop "(1 << 40)")
elseif(KernelArmCortexA55)
    set(KernelArmICacheVIPT ON)
    set(KernelArmPASizeBits40 ON)
    math(EXPR KernelPaddrUserTop "(1 << 40)")
elseif(KernelArmCortexA57)
    set(KernelArmPASizeBits44 ON)
    math(EXPR KernelPaddrUserTop "(1 << 44)")
elseif(KernelArmCortexA72)
    # For Cortex-A72 in AArch64 state, the physical address range is 44 bits
    # (https://developer.arm.com/documentation/100095/0001/memory-management-unit/about-the-mmu)
    set(KernelArmPASizeBits44 ON)
    math(EXPR KernelPaddrUserTop "(1 << 44)")
endif()
config_set(KernelArmPASizeBits40 ARM_PA_SIZE_BITS_40 "${KernelArmPASizeBits40}")
config_set(KernelArmPASizeBits44 ARM_PA_SIZE_BITS_44 "${KernelArmPASizeBits44}")
config_set(KernelArmICacheVIPT ARM_ICACHE_VIPT "${KernelArmICacheVIPT}")

if(KernelSel4ArchAarch32)
    # 64-bit targets may be building in 32-bit mode,
    # so make sure maximum paddr is 32-bit.
    math(EXPR KernelPaddrUserTop "(1 << 32) - 1")
endif()

include(src/arch/arm/armv/armv7-a/config.cmake)
include(src/arch/arm/armv/armv8-a/config.cmake)

config_option(
    KernelDebugDisableL2Cache DEBUG_DISABLE_L2_CACHE
    "Do not enable the L2 cache on startup for debugging purposes."
    DEFAULT OFF
    DEPENDS "KernelArchARM"
)
config_option(
    KernelDebugDisableL1ICache DEBUG_DISABLE_L1_ICACHE
    "Do not enable the L1 instruction cache on startup for debugging purposes."
    DEFAULT OFF
    DEPENDS "KernelArchARM;KernelDebugDisableL2Cache"
)
config_option(
    KernelDebugDisableL1DCache DEBUG_DISABLE_L1_DCACHE
    "Do not enable the L1 data cache on startup for debugging purposes."
    DEFAULT OFF
    DEPENDS "KernelArchARM;KernelDebugDisableL2Cache"
)
config_option(
    KernelDebugDisableBranchPrediction DEBUG_DISABLE_BRANCH_PREDICTION
    "Do not enable branch prediction (also called program flow control) on startup. \
    This makes execution time more deterministic at the expense of dramatically decreasing \
    performance. Primary use is for debugging."
    DEFAULT OFF
    DEPENDS "KernelArchARM"
)

if(NOT DEFINED KernelSel4ArchArmHyp)
    # the current CMake scripts ensure that KernelSel4ArchArmHyp is always set
    # to either ON or OFF. If it is not set, something is either broken or the
    # CMake files are used wrongly. Or support for KernelSel4ArchArmHyp has
    # finally been removed - and then this check here should be removed and the
    # KernelArmHypervisorSupport below can be OFF by default.
    message(FATAL_ERROR "KernelSel4ArchArmHyp must be ON or OFF")
endif()
config_option(
    KernelArmHypervisorSupport ARM_HYPERVISOR_SUPPORT
    "Build as Hypervisor. Utilise ARM virtualisation extensions to build the kernel as a hypervisor"
    DEFAULT ${KernelSel4ArchArmHyp}
    DEPENDS
        "KernelArmCortexA15 OR KernelArmCortexA35 OR KernelArmCortexA57 OR KernelArmCortexA53 OR KernelArmCortexA55 OR KernelArmCortexA72"
)

config_option(KernelArmGicV3 ARM_GIC_V3_SUPPORT "Build support for GICv3" DEFAULT OFF)

if(KernelArmPASizeBits40 AND ARM_HYPERVISOR_SUPPORT)
    config_set(KernelAarch64VspaceS2StartL1 AARCH64_VSPACE_S2_START_L1 "ON")
else()
    config_set(KernelAarch64VspaceS2StartL1 AARCH64_VSPACE_S2_START_L1 "OFF")
endif()

config_option(
    KernelArmHypEnableVCPUCP14SaveAndRestore ARM_HYP_ENABLE_VCPU_CP14_SAVE_AND_RESTORE
    "Trap, but don't save/restore VCPUs' CP14 accesses \
    This allows us to turn off the save and restore of VCPU threads' CP14 \
    context for performance (or other) reasons, we can just turn them off \
    and trap them instead, and have the VCPUs' accesses to CP14 \
    intercepted and delivered to the VM Monitor as fault messages"
    DEFAULT ON
    DEPENDS "KernelSel4ArchArmHyp;NOT KernelVerificationBuild"
    DEFAULT_DISABLED OFF
)

config_option(
    KernelArmErrata430973 ARM_ERRATA_430973
    "Enable workaround for 430973 Cortex-A8 (r1p0..r1p2) erratum \
    Enables a workaround for the 430973 Cortex-A8 (r1p0..r1p2) erratum. Error occurs \
    if code containing ARM/Thumb interworking branch is replaced by different code \
    at the same virtual address."
    DEFAULT OFF
    DEPENDS "KernelArchARM;KernelArmCortexA8"
)

config_option(
    KernelArmErrata773022 ARM_ERRATA_773022
    "Enable workaround for 773022 Cortex-A15 (r0p0..r0p4) erratum \
    Enables a workaround for the 773022 Cortex-A15 (r0p0..r0p4) erratum. Error occurs \
    on rare sequences of instructions and results in the loop buffer delivering \
    incorrect instructions. The work around is to disable the loop buffer"
    DEFAULT ON
    DEPENDS "KernelArchARM;KernelArmCortexA15"
    DEFAULT_DISABLED OFF
)

config_option(KernelArmSMMU ARM_SMMU "Enable SystemMMU" DEFAULT OFF DEPENDS "KernelPlatformTx2")

config_option(
    KernelTk1SMMU TK1_SMMU "Enable SystemMMU for the Tegra TK1 SoC"
    DEFAULT OFF
    DEPENDS "KernelPlatformTK1"
)

config_option(KernelArmEnableA9Prefetcher ENABLE_A9_PREFETCHER "Enable Cortex-A9 prefetcher \
    Cortex-A9 has an L1 and L2 prefetcher. By default \
    they are disabled. This config options allows \
    them to be turned on. Enabling the prefetchers \
    requires that the kernel be in secure mode. ARM \
    documents indicate that as of r4p1 version of \
    Cortex-A9 the bits used to enable the prefetchers \
    no longer exist, it is not clear if this is just \
    a document error or not." DEFAULT OFF DEPENDS "KernelArmCortexA9")

config_option(
    KernelArmExportPMUUser EXPORT_PMU_USER "PL0 access to PMU. \
    Grant user access to Performance Monitoring Unit. \
    WARNING: While useful for evaluating performance, \
    this option opens timing and covert channels."
    DEFAULT OFF
    DEPENDS "KernelArchArmV7a OR KernelArchArmV8a;NOT KernelArmCortexA8"
)

config_option(
    KernelArmDisableWFIWFETraps DISABLE_WFI_WFE_TRAPS "Disable the trapping of WFI \
    and WFE instructions when configuring the \
    Hyp Configuration Registor (HCR) of a VCPU"
    DEFAULT OFF
    DEPENDS "KernelArchArmV7a OR KernelArchArmV8a;KernelArmHypervisorSupport"
)
config_option(KernelTk1SMMUInterruptEnable SMMU_INTERRUPT_ENABLE "Enable SMMU interrupts. \
    SMMU interrupts currently only serve a debug purpose as \
    they are not forwarded to user level. Enabling this will \
    cause some fault types to print out a message in the kernel. \
    WARNING: Printing fault information is slow and rapid faults \
    can result in all time spent in the kernel printing fault \
    messages" DEFAULT "${KernelDebugBuild}" DEPENDS "KernelTk1SMMU" DEFAULT_DISABLED OFF)

config_option(
    KernelAArch32FPUEnableContextSwitch AARCH32_FPU_ENABLE_CONTEXT_SWITCH
    "Enable hardware VFP and SIMD context switch \
        This enables the VFP and SIMD context switch on platforms with \
        hardware support, allowing the user to execute hardware VFP and SIMD \
        operations in a multithreading environment, instead of relying on \
        software emulation of FPU/VFP from the C library (e.g. mfloat-abi=soft)."
    DEFAULT ON
    DEPENDS "KernelSel4ArchAarch32;NOT KernelVerificationBuild"
    DEFAULT_DISABLED OFF
)

config_option(
    KernelAArch64SErrorIgnore AARCH64_SERROR_IGNORE
    "By default any SError interrupt will halt the kernel. SErrors may \
    be caused by e.g. writes to read-only device registers or ECC errors. \
    When this option is enabled SErrors will be ignored."
    DEFAULT OFF
    DEPENDS "KernelSel4ArchAarch64;NOT KernelVerificationBuild"
)
mark_as_advanced(KernelAArch64SErrorIgnore)

if(KernelAArch32FPUEnableContextSwitch OR KernelSel4ArchAarch64)
    set(KernelHaveFPU ON)
endif()

if(KernelSel4ArchAarch64)
    set(KernelHardwareDebugAPIUnsupported ON CACHE INTERNAL "")
endif()

if(
    KernelArmCortexA7
    OR KernelArmCortexA8
    OR KernelArmCortexA15
    OR KernelArmCortexA35
    OR KernelArmCortexA53
    OR KernelArmCortexA55
    OR KernelArmCortexA57
    OR KernelArmCortexA72
)
    # According to https://developer.arm.com/documentation/100095/0001/functional-description/about-the-cortex-a72-processor-functions/components-of-the-processor
    # the L1 instruction on the Cortex-A72 cache has a 64-byte cache line.
    # Thus, 6 bits are needed.
    config_set(KernelArmCacheLineSizeBits L1_CACHE_LINE_SIZE_BITS "6")
elseif(KernelArmCortexA9)
    config_set(KernelArmCacheLineSizeBits L1_CACHE_LINE_SIZE_BITS "5")
endif()

add_sources(
    DEP "KernelArchARM"
    PREFIX src/arch/arm
    CFILES
        c_traps.c
        api/faults.c
        benchmark/benchmark.c
        kernel/boot.c
        kernel/thread.c
        machine/cache.c
        machine/errata.c
        machine/debug.c
        machine/hardware.c
        machine/io.c
        object/interrupt.c
        object/tcb.c
        object/iospace.c
        object/vcpu.c
        object/smmu.c
        smp/ipi.c
)

add_bf_source_old("KernelArchARM" "structures.bf" "include/arch/arm" "arch/object")

include(src/arch/arm/${KernelWordSize}/config.cmake)
