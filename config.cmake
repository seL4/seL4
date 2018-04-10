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

set(configure_string "")

# Set kernel branch
config_set(KernelIsMaster KERNEL_MASTER ON)

# Proof based configuration variables
set(CSPEC_DIR "." CACHE PATH "")
set(SKIP_MODIFIES ON CACHE BOOL "")
set(SORRY_BITFIELD_PROOFS OFF CACHE BOOL "")
find_file(UMM_TYPES umm_types.txt CMAKE_FIND_ROOT_PATH_BOTH)
set(force FORCE)
if(KernelVerificationBuild)
    set(force CLEAR)
endif()
mark_as_advanced(${force} CSPEC_DIR SKIP_MODIFIES SORRY_BITFIELD_PROOFS UMM_TYPES)
# Use a custom target for collecting information during generation that we need during build
add_custom_target(kernel_config_target)
# Put our common top level types in
set_property(TARGET kernel_config_target APPEND PROPERTY TOPLEVELTYPES
    cte_C tcb_C endpoint_C notification_C asid_pool_C pte_C pde_C user_data_C
    user_data_device_C
)

########################
# Architecture selection
########################
config_choice(KernelArch ARCH "Architecture to use when building the kernel"
    "arm;KernelArchARM;ARCH_ARM"
    "riscv;KernelArchRiscV;ARCH_RISCV"
    "x86;KernelArchX86;ARCH_X86"
)

# Set defaults for common variables
set(KernelHaveFPU OFF)

include(src/arch/arm/config.cmake)
include(src/arch/riscv/config.cmake)
include(src/arch/x86/config.cmake)

include(include/32/mode/config.cmake)
include(include/64/mode/config.cmake)
include(src/config.cmake)

# Enshrine common variables in the config
config_set(KernelHaveFPU HAVE_FPU "${KernelHaveFPU}")

# System parameters
config_string(KernelRootCNodeSizeBits ROOT_CNODE_SIZE_BITS
    "Root CNode Size (2^n slots) \
    The acceptable range is 4-27, based on the kernel-supplied caps.\
    The root CNode needs at least enough space to contain up to\
    BI_CAP_DYN_START. Note that in practice your root CNode will need\
    to be several bits larger than 4 to fit untyped caps and\
    cannot be 27 bits as it won't fit in memory."
    DEFAULT 12
    UNQUOTE
)

config_string(KernelTimerTickMS TIMER_TICK_MS
    "Timer tick period in milliseconds"
    DEFAULT 2
    UNQUOTE
)
config_string(KernelTimeSlice TIME_SLICE
    "Number of timer ticks until a thread is preempted."
    DEFAULT 5
    UNQUOTE
)
config_string(KernelRetypeFanOutLimit RETYPE_FAN_OUT_LIMIT
    "Maximum number of objects that can be created in a single Retype() invocation."
    DEFAULT 256
    UNQUOTE
)
config_string(KernelMaxNumWorkUnitsPerPreemption MAX_NUM_WORK_UNITS_PER_PREEMPTION
    "Maximum number of work units (delete/revoke iterations) until the kernel checks for\
    pending interrupts (and preempts the currently running syscall if interrupts are pending)."
    DEFAULT 100
    UNQUOTE
)
config_string(KernelResetChunkBits RESET_CHUNK_BITS
    "Maximum size in bits of chunks of memory to zero before checking a preemption point."
    DEFAULT 8
    UNQUOTE
)
config_string(KernelMaxNumBootinfoUntypedCaps MAX_NUM_BOOTINFO_UNTYPED_CAPS
    "Max number of bootinfo untyped caps"
    DEFAULT 230
    UNQUOTE
)
config_option(KernelFastpath FASTPATH "Enable IPC fastpath" DEFAULT ON)

config_string(KernelNumDomains NUM_DOMAINS "The number of scheduler domains in the system" DEFAULT 1 UNQUOTE)

find_file(KernelDomainSchedule default_domain.c PATHS src/config CMAKE_FIND_ROOT_PATH_BOTH
    DOC "A C file providing the symbols ksDomSchedule and ksDomeScheudleLength \
        to be linked with the kernel as a scheduling configuration."
)

config_string(KernelNumPriorities NUM_PRIORITIES
    "The number of priority levels per domain. Valid range 1-256"
    DEFAULT 256
    UNQUOTE
)

config_string(KernelMaxNumNodes MAX_NUM_NODES "Max number of CPU cores to boot"
    DEFAULT 1
    DEPENDS "${KernelNumDomains} EQUAL 1;NOT KernelArchRiscV"
    UNQUOTE
)

config_string(KernelStackBits KERNEL_STACK_BITS
    "This describes the log2 size of the kernel stack. Great care should be taken as\
    there is no guard below the stack so setting this too small will cause random\
    memory corruption"
    DEFAULT 12
    UNQUOTE
)

config_string(KernelFPUMaxRestoresSinceSwitch FPU_MAX_RESTORES_SINCE_SWITCH
    "This option is a heuristic to attempt to detect when the FPU is no longer in use,\
    allowing the kernel to save the FPU state out so that the FPU does not have to be\
    enabled/disabled every thread swith. Every time we restore a thread and there is\
    active FPU state, we increment this setting and if it exceeds this threshold we\
    switch to the NULL state."
    DEFAULT 64
    DEPENDS "KernelHaveFPU" UNDEF_DISABLED
    UNQUOTE
)

config_option(KernelVerificationBuild VERIFICATION_BUILD
    "When enabled this configuration option prevents the usage of any other options that\
    would compromise the verification story of the kernel. Enabling this option does NOT\
    imply you are using a verified kernel."
    DEFAULT ON
)

config_option(KernelDebugBuild DEBUG_BUILD
    "Enable debug facilities (symbols and assertions) in the kernel"
    DEFAULT ON
    DEPENDS "NOT KernelVerificationBuild" DEFAULT_DISABLED OFF
)

config_option(HardwareDebugAPI HARDWARE_DEBUG_API
    "Builds the kernel with support for a userspace debug API, which can \
    allows userspace processes to set breakpoints, watchpoints and to \
    single-step through thread execution."
    DEFAULT OFF
    DEPENDS "NOT KernelVerificationBuild"
)
config_option(KernelPrinting PRINTING
    "Allow the kernel to print out messages to the serial console during bootup and execution."
    DEFAULT "${KernelDebugBuild}"
    DEPENDS "NOT KernelVerificationBuild" DEFAULT_DISABLED OFF
)
config_choice(KernelBenchmarks KERNEL_BENCHMARK "Enable benchamrks including logging and tracing info. \
    Setting this value > 1 enables a 1MB log buffer and functions for extracting data from it \
    at user level. NOTE this is only tested on the sabre and will not work on platforms with < 512mb memory. \
    This is not fully implemented for x86. \
    none -> No Benchmarking features enabled. \
    generic -> Enable global benchmarks config variable with no specific features. \
    track_kernel_entries -> Log kernel entries information including timing, number of invocations and arguments for \
    system calls, interrupts, user faults and VM faults. \
    tracepoints -> Enable manually inserted tracepoints that the kernel will track time consumed between. \
    track_utilisation -> Enable the kernel to track each thread's utilisation time."
    "none;KernelBenchmarksNone;NO_BENCHMARKS"
    "generic;KernelBenchmarksGeneric;BENCHMARK_GENERIC;NOT KernelVerificationBuild"
    "track_kernel_entries;KernelBenchmarksTrackKernelEntries;BENCHMARK_TRACK_KERNEL_ENTRIES;NOT KernelVerificationBuild"
    "tracepoints;KernelBenchmarksTracepoints;BENCHMARK_TRACEPOINTS;NOT KernelVerificationBuild"
    "track_utilisation;KernelBenchmarksTrackUtilisation;BENCHMARK_TRACK_UTILISATION;NOT KernelVerificationBuild"
)
if(NOT (KernelBenchmarks STREQUAL "none"))
    config_set(KernelEnableBenchmarks ENABLE_BENCHMARKS ON)
else()
    config_set(KernelEnableBenchmarks ENABLE_BENCHMARKS OFF)
endif()
config_string(KernelMaxNumTracePoints MAX_NUM_TRACE_POINTS
    "Use TRACE_POINT_START(k) and TRACE_POINT_STOP(k) macros for recording data, \
    where k is an integer between 0 and this value - 1. The maximum number of \
    different trace point identifiers which can be used."
    DEFAULT 1
    DEPENDS "NOT KernelVerificationBuild;KernelBenchmarksTracepoints" DEFAULT_DISABLED 0
    UNQUOTE
)
# TODO: this config has no business being in the build system, and should
# be moved to C headers, but for now must be emulated here for compatibility
if(KernelBenchmarksTrackKernelEntries OR KernelBenchmarksTracepoints)
    config_set(KernelBenchmarkUseKernelLogBuffer BENCHMARK_USE_KERNEL_LOG_BUFFER ON)
else()
    config_set(KernelBenchmarkUseKernelLogBuffer BENCHMARK_USE_KERNEL_LOG_BUFFER OFF)
endif()

config_option(KernelIRQReporting IRQ_REPORTING
    "seL4 does not properly check for and handle spurious interrupts. This can result \
    in unnecessary output from the kernel during debug builds. If you are CERTAIN these \
    messages are benign then use this config to turn them off."
    DEFAULT ON
    DEPENDS "KernelPrinting" DEFAULT_DISABLED OFF
)
config_option(KernelColourPrinting COLOUR_PRINTING
    "In debug mode, seL4 prints diagnostic messages to its serial output describing, \
    e.g., the cause of system call errors. This setting determines whether ANSI escape \
    codes are applied to colour code these error messages. You may wish to disable this \
    setting if your serial output is redirected to a file or pipe."
    DEFAULT ON
    DEPENDS "KernelPrinting" DEFAULT_DISABLED OFF
)
config_string(KernelUserStackTraceLength USER_STACK_TRACE_LENGTH
    "On a double fault the kernel can try and print out the users stack to aid \
    debugging. This option determines how many words of stack should be printed."
    DEFAULT 16
    DEPENDS "KernelPrinting" DEFAULT_DISABLED 0
    UNQUOTE
)

config_choice(KernelOptimisation KERNEL_OPT_LEVEL "Select the kernel optimisation level"
    "-O2;KerenlOptimisationO2;KERNEL_OPT_LEVEL_O2"
    "-Os;KerenlOptimisationOS;KERNEL_OPT_LEVEL_OS"
    "-O1;KerenlOptimisationO1;KERNEL_OPT_LEVEL_O1"
    "-O3;KerenlOptimisationO3;KERNEL_OPT_LEVEL_O3"
)

config_option(KernelFWholeProgram KERNEL_FWHOLE_PROGRAM
    "Enable -fwhole-program when linking kernel. This should work modulo gcc bugs, which \
    are not uncommon with -fwhole-program. Consider this feature experimental!"
    DEFAULT OFF
)


config_option(KernelDangerousCodeInjection DANGEROUS_CODE_INJECTION
    "Adds a system call that allows users to specify code to be run in kernel mode. \
    Useful for profiling."
    DEFAULT OFF
    DEPENDS "NOT KernelARMHypervisorSupport;NOT KernelVerificationBuild;NOT KernelPlatformHikey;NOT KernelSkimWindow"
)

config_option(KernelDebugDisablePrefetchers DEBUG_DISABLE_PREFETCHERS
    "On ia32 platforms, this option disables the L2 hardware prefetcher, the L2 adjacent \
    cache line prefetcher, the DCU prefetcher and the DCU IP prefetcher. On the cortex \
    a53 this disables the L1 Data prefetcher."
    DEFAULT OFF
    DEPENDS "KernelArchX86 OR KernelPlatformHikey"
)

add_config_library(kernel "${configure_string}")
