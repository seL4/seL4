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

config_choice(KernelArmSel4Arch ARM_SEL4_ARCH "Architecture mode for building the kernel"
    "aarch32;KernelSel4ArchAarch32;ARCH_AARCH32;KernelArchARM"
    "aarch64;KernelSel4ArchAarch64;ARCH_AARCH64;KernelArchARM"
    "arm_hyp;KernelSel4ArchArmHyp;ARCH_ARM_HYP;KernelArchARM;KernelArmHypervisorSupport"
)

config_choice(KernelARMPlatform ARM_PLAT "Select the platform for the architecture"
    "sabre;KernelPlatformSabre;PLAT_SABRE;KernelSel4ArchAarch32"
    "kzm;KernelPlatformKZM;PLAT_KZM;KernelSel4ArchAarch32"
    "omap3;KernelPlatformOMAP3;PLAT_OMAP3;KernelSel4ArchAarch32"
    "am335x;KernelPlatformAM335X;PLAT_AM335X;KernelSel4ArchAarch32"
    "exynos4;KernelPlatformExynos4;PLAT_EXYNOS4;KernelSel4ArchAarch32"
    "exynos5410;KernelPlatformExynos5410;PLAT_EXYNOS5410;KernelSel4ArchAarch32 OR KernelSel4ArchArmHyp"
    "exynos5422;KernelPlatformExynos5422;PLAT_EXYNOS5422;KernelSel4ArchAarch32"
    "exynos5250;KernelPlatformExynos5250;PLAT_EXYNOS5250;KernelSel4ArchAarch32"
    "apq8064;KernelPlatformAPQ8064;PLAT_APQ8064;KernelSel4ArchAarch32"
    "wandq;KernelPlatformWandQ;PLAT_WANDQ;KernelSel4ArchAarch32"
    "imx7sabre;KernelPlatformImx7Sabre;PLAT_IMX7_SABRE;KernelSel4ArchAarch32"
    "zynq7000;KernelPlatformZynq7000;PLAT_ZYNQ7000;KernelSel4ArchAarch32"
    "allwinnera20;KernelPlatformAllwinnerA20;PLAT_ALLWINNERA20;KernelSel4ArchAarch32"
    "tk1;KernelPlatformTK1;PLAT_TK1;KernelSel4ArchAarch32 OR KernelSel4ArchArmHyp"
    "hikey;KernelPlatformHikey;PLAT_HIKEY;KernelArchARM"
    "rpi3;KernelPlatformRpi3;PLAT_BCM2837;KernelSel4ArchAarch32"
    "tx1;KernelPlatformTx1;PLAT_TX1;KernelSel4ArchAarch64"
)

if(KernelArchARM)
    config_set(KernelSel4Arch SEL4_ARCH "${KernelArmSel4Arch}")
endif()

# arm-hyp masquerades as an aarch32 build
if(KernelSel4ArchArmHyp)
    config_set(KernelSel4ArmHypAarch32 ARCH_AARCH32 ON)
    set(KernelSel4ArchAarch32 ON CACHE INTERNAL "")
else()
    config_set(KernelSel4ArmHypAarch32 ARCH_AARCH32 OFF)
endif()

if(KernelSel4ArchAarch32 OR KernelSel4ArchArmHyp)
    set_kernel_32()
elseif(KernelSel4ArchAarch64)
    set_kernel_64()
endif()

# Include all the platforms. For all of the common variables we set a default value here
# and let the platforms override them.
set(KernelArmMachFeatureModifiers "" CACHE INTERNAL "")
set(KernelArmMach "" CACHE INTERNAL "")
set(KernelArmCortexA7 OFF)
set(KernelArmCortexA8 OFF)
set(KernelArmCortexA9 OFF)
set(KernelArmCortexA15 OFF)
set(KernelArmCortexA53 OFF)
set(KernelArmCortexA57 OFF)
set(KernelArchArmV6 OFF)
set(KernelArchArmV7a OFF)
set(KernelArchArmV7ve OFF)
set(KernelArchArmV8a OFF)
set(KernelArm1136JF_S OFF)
include(src/plat/imx6/config.cmake)
include(src/plat/imx7/config.cmake)
include(src/plat/imx31/config.cmake)
include(src/plat/omap3/config.cmake)
include(src/plat/exynos4/config.cmake)
include(src/plat/exynos5/config.cmake)
include(src/plat/am335x/config.cmake)
include(src/plat/hikey/config.cmake)
include(src/plat/apq8064/config.cmake)
include(src/plat/bcm2837/config.cmake)
include(src/plat/tk1/config.cmake)
include(src/plat/tx1/config.cmake)
include(src/plat/zynq7000/config.cmake)

# Now enshrine all the common variables in the config
config_set(KernelArmCortexA7 ARM_CORTEX_A7 "${KernelArmCortexA7}")
config_set(KernelArmCortexA8 ARM_CORTEX_A8 "${KernelArmCortexA8}")
config_set(KernelArmCortexA9 ARM_CORTEX_A9 "${KernelArmCortexA9}")
config_set(KernelArmCortexA15 ARM_CORTEX_A15 "${KernelArmCortexA15}")
config_set(KernelArmCortexA53 ARM_CORTEX_A53 "${KernelArmCortexA53}")
config_set(KernelArmCortexA57 ARM_CORTEX_A57 "${KernelArmCortexA57}")
config_set(KernelArm1136JF_S ARM1136JF_S "${KernelArm1136JF_S}")
config_set(KernelArchArmV6 ARCH_ARM_V6 "${KernelArchArmV6}")
config_set(KernelArchArmV7a ARCH_ARM_V7A "${KernelArchArmV7a}")
config_set(KernelArchArmV7ve ARCH_ARM_V7VE "${KernelArchArmV7ve}")
config_set(KernelArchArmV78a ARCH_ARM_V8A "${KernelArchArmV8a}")

set(KernelArmCPU "" CACHE INTERNAL "")
set(KernelArmArmV "" CACHE INTERNAL "")

# Check for v7ve before v7a as v7ve is a superset and we want to set the
# actual armv to that, but leave armv7a config enabled for anything that
# checks directly against it
if(KernelArchArmV7ve)
    set(KernelArmArmV "armv7ve" CACHE INTERNAL "")
elseif(KernelArchArmV7a)
    set(KernelArmArmV "armv7-a" CACHE INTERNAL "")
elseif(KernelArchArmV8a)
    set(KernelArmArmV "armv8-a" CACHE INTERNAL "")
elseif(KernelArchArmV6)
    set(KernelArmArmV "armv6" CACHE INTERNAL "")
endif()
if(KernelArmCortexA7)
    set(KernelArmCPU "cortex-a7" CACHE INTERNAL "")
elseif(KernelArmCortexA8)
    set(KernelArmCPU "cortex-a8" CACHE INTERNAL "")
elseif(KernelArmCortexA9)
    set(KernelArmCPU "cortex-a9" CACHE INTERNAL "")
elseif(KernelArmCortexA15)
    set(KernelArmCPU "cortex-a15" CACHE INTERNAL "")
elseif(KernelArmCortexA53)
    set(KernelArmCPU "cortex-a53" CACHE INTERNAL "")
elseif(KernelArmCortexA57)
    set(KernelArmCPU "cortex-a57" CACHE INTERNAL "")
elseif(KernelArm1136JF_S)
    set(KernelArmCPU "arm1136jf-s" CACHE INTERNAL "")
endif()
if(KernelArchARM)
    config_set(KernelArmMach ARM_MACH "${KernelArmMach}")
endif()

include(src/arch/arm/armv/armv6/config.cmake)
include(src/arch/arm/armv/armv7-a/config.cmake)
include(src/arch/arm/armv/armv8-a/config.cmake)

config_choice(KernelIPCBufferLocation KERNEL_IPC_BUFFER_LOCATION
    "Controls how the location of the IPC buffer is provided to the user \
    globals_frame-> Put the address of the IPC buffer in a dedicated frame that is \
        read only at user level. This works on all ARM platforms \
    threadID_register-> Put the address of the IPC buffer in the user readable/writeable \
        ThreadID register. When enabled this has the result of the kernel overwriting \
        any value the user writes to this register."
    "threadID_register;KernelIPCBufferThreadID;IPC_BUF_TPIDRURW;KernelArchARM;NOT KernelArchArmV6"
    "globals_frame;KernelIPCBufferGlobalsFrame;IPC_BUF_GLOBALS_FRAME;KernelSel4ArchAarch32"
)

config_option(KernelDangerousCodeInjectionOnUndefInstr DANGEROUS_CODE_INJECTION_ON_UNDEF_INSTR
    "Replaces the undefined instruction handler with a call to a function pointer in r8. \
    This is an alternative mechanism to the code injection syscall. On ARMv6 the syscall \
    interferes with the caches and branch predictor in such a way that it is unsuitable \
    for benchmarking. This option has no effect on non-ARMv6 platforms."
    DEFAULT OFF
    DEPENDS "KernelArchArmV6;NOT KernelVerificationBuild"
)

config_option(KernelDebugDisableL2Cache DEBUG_DISABLE_L2_CACHE
    "Do not enable the L2 cache on startup for debugging purposes."
    DEFAULT OFF
    DEPENDS "KernelArchARM"
)
config_option(KernelDebugDisableL1ICache DEBUG_DISABLE_L1_ICACHE
    "Do not enable the L1 instruction cache on startup for debugging purposes."
    DEFAULT OFF
    DEPENDS "KernelArchARM;KernelDebugDisableL2Cache"
)
config_option(KernelDebugDisableL1DCache DEBUG_DISABLE_L1_DCACHE
    "Do not enable the L1 data cache on startup for debugging purposes."
    DEFAULT OFF
    DEPENDS "KernelArchARM;KernelDebugDisableL2Cache"
)
config_option(KernelDebugDisableBranchPrediction DEBUG_DISABLE_BRANCH_PREDICTION
    "Do not enable branch prediction (also called program flow control) on startup. \
    This makes execution time more deterministic at the expense of dramatically decreasing \
    performance. Primary use is for debugging."
    DEFAULT OFF
    DEPENDS "KernelArchARM"
)

config_option(KernelArmHypervisorSupport ARM_HYPERVISOR_SUPPORT
    "Build as Hypervisor. Utilise ARM virtualisation extensions to build the kernel as a hypervisor"
    DEFAULT OFF
    DEPENDS "KernelArmCortexA15"
)

config_option(KernelArmHypEnableVCPUCP14SaveAndRestore ARM_HYP_ENABLE_VCPU_CP14_SAVE_AND_RESTORE
    "Trap, but don't save/restore VCPUs' CP14 accesses \
    This allows us to turn off the save and restore of VCPU threads' CP14 \
    context for performance (or other) reasons, we can just turn them off \
    and trap them instead, and have the VCPUs' accesses to CP14 \
    intercepted and delivered to the VM Monitor as fault messages"
    DEFAULT ON
    DEPENDS "KernelArmHypervisorSupport;NOT KernelVerificationBuild" DEFAULT_DISABLED OFF
)

config_option(KernelArmErrata430973 ARM_ERRATA_430973
    "Enable workaround for 430973 Cortex-A8 (r1p0..r1p2) erratum \
    Enables a workaround for the 430973 Cortex-A8 (r1p0..r1p2) erratum. Error occurs \
    if code containing ARM/Thumb interworking branch is replaced by different code \
    at the same virtual address."
    DEFAULT OFF
    DEPENDS "KernelArchARM;KernelArmCortexA8"
)

config_option(KernelArmErrata773022 ARM_ERRATA_773022
    "Enable workaround for 773022 Cortex-A15 (r0p0..r0p4) erratum \
    Enables a workaround for the 773022 Cortex-A15 (r0p0..r0p4) erratum. Error occurs \
    on rare sequences of instructions and results in the loop buffer delivering \
    incorrect instructions. The work around is to disable the loop buffer"
    DEFAULT ON
    DEPENDS "KernelArchARM;KernelArmCortexA15" DEFAULT_DISABLED OFF
)

config_option(KernelArmSMMU ARM_SMMU
    "Enable SystemMMU for the Tegra TK1 SoC"
    DEFAULT OFF
    DEPENDS "KernelPlatformTK1"
)

config_option(KernelArmEnableA9Prefetcher ENABLE_A9_PREFETCHER
    "Enable Cortex-A9 prefetcher \
    Cortex-A9 has an L1 and L2 prefetcher. By default \
    they are disabled. This config options allows \
    them to be turned on. Enabling the prefetchers \
    requires that the kernel be in secure mode. ARM \
    documents indicate that as of r4p1 version of \
    Cortex-A9 the bits used to enable the prefetchers \
    no longer exist, it is not clear if this is just \
    a document error or not."
    DEFAULT OFF
    DEPENDS "KernelArmCortexA9"
)

config_option(KernelArmExportPMUUser EXPORT_PMU_USER
    "PL0 access to PMU. \
    Grant user access to Performance Monitoring Unit. \
    WARNING: While useful for evaluating performance, \
    this option opens timing and covert channels."
    DEFAULT OFF
    DEPENDS "KernelArchArmV7a OR KernelArchArmV8a;NOT KernelArmCortexA8"
)

config_option(KernelArmExportPCNTUser EXPORT_PCNT_USER
    "PL0 access to generic timer CNTPCT and CNTFRQ. \
    Grant user access to physical counter and counter \
    frequency registers of the generic timer. \
    WARNING: selecting this option opens a timing \
    channel"
    DEFAULT OFF
    DEPENDS "KernelArmCortexA15"
)

config_option(KernelArmExportVCNTUser EXPORT_VCNT_USER
    "PL0 access to generic timer CNTVCT and CNTFRQ. \
    Grant user access to virtual counter and counter \
    frequency registers of the generic timer. \
    WARNING: selecting this option opens a timing \
    channel"
    DEFAULT OFF
    DEPENDS "KernelArmCortexA15"
)

config_option(KernelARMSMMUInterruptEnable SMMU_INTERRUPT_ENABLE
    "Enable SMMU interrupts. \
    SMMU interrupts currently only serve a debug purpose as \
    they are not forwarded to user level. Enabling this will \
    cause some fault types to print out a message in the kernel. \
    WARNING: Printing fault information is slow and rapid faults \
    can result in all time spent in the kernel printing fault \
    messages"
    DEFAULT "${KernelDebugBuild}"
    DEPENDS "KernelArmSMMU" DEFAULT_DISABLED OFF
)

config_option(KernelAArch32FPUEnableContextSwitch AARCH32_FPU_ENABLE_CONTEXT_SWITCH
    "Enable hardware VFP and SIMD context switch \
        This enables the VFP and SIMD context switch on platforms with \
        hardware support, allowing the user to execute hardware VFP and SIMD \
        operations in a multithreading environment, instead of relying on \
        software emulation of FPU/VFP from the C library (e.g. mfloat-abi=soft)."
    DEFAULT ON
    DEPENDS "KernelSel4ArchAarch32;NOT KernelArchArmV6;NOT KernelVerificationBuild"
    DEFAULT_DISABLED OFF
)

if(KernelAArch32FPUEnableContextSwitch)
    set(KernelHaveFPU ON)
endif()

# TODO: this config has no business being in the build system, and should
# be moved to C headers, but for now must be emulated here for compatibility
if(KernelBenchmarksTrackUtilisation AND KernelArchARM)
    config_set(KernelArmEnablePMUOverflowInterrupt ARM_ENABLE_PMU_OVERFLOW_INTERRUPT ON)
else()
    config_set(KernelArmEnablePMUOverflowInterrupt ARM_ENABLE_PMU_OVERFLOW_INTERRUPT OFF)
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
        machine/io.c
        machine/debug.c
        object/interrupt.c
        object/tcb.c
        object/iospace.c
        object/vcpu.c
        smp/ipi.c
)

add_sources(
    DEP "KernelArmCortexA9"
    CFILES src/arch/arm/machine/l2c_310.c
)

add_sources(
    DEP "KernelArmCortexA9;NOT KernelPlatformExynos4"
    CFILES src/arch/arm/machine/priv_timer.c
)

add_sources(
    DEP "KernelArmCortexA15 OR KernelArmCortexA7 OR KernelArmCortexA57 OR KernelArmCortexA9 OR KernelPlatformHikey"
    CFILES src/arch/arm/machine/gic_pl390.c
)

add_bf_source_old("KernelArchARM" "structures.bf" "include/arch/arm" "arch/object")
add_bf_source_old("KernelArchARM" "hardware.bf" "include/plat/${KernelPlatform}" "plat/machine")

include(src/arch/arm/32/config.cmake)
include(src/arch/arm/64/config.cmake)
