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
set_property(
    TARGET kernel_config_target
    APPEND
    PROPERTY
        TOPLEVELTYPES
        cte_C
        tcb_C
        endpoint_C
        notification_C
        asid_pool_C
        pte_C
        user_data_C
        user_data_device_C
)

# Create and set all of the Kernel config options that can
# be derived from the seL4 arch which is one of the following:
# aarch32, aarch64, arm_hyp, riscv32, riscv64, x86_64, ia32
# This macro is intended to be called from within a platform config.
macro(declare_seL4_arch sel4_arch)
    set(KernelSel4Arch "${sel4_arch}" CACHE STRING "")
    config_choice(
        KernelSel4Arch
        SEL4_ARCH
        "Architecture mode for building the kernel"
        "aarch32;KernelSel4ArchAarch32;ARCH_AARCH32"
        "aarch64;KernelSel4ArchAarch64;ARCH_AARCH64"
        "arm_hyp;KernelSel4ArchArmHyp;ARCH_ARM_HYP"
        "riscv32;KernelSel4ArchRiscV32;ARCH_RISCV32"
        "riscv64;KernelSel4ArchRiscV64;ARCH_RISCV64"
        "x86_64;KernelSel4ArchX86_64;ARCH_X86_64"
        "ia32;KernelSel4ArchIA32;ARCH_IA32"
    )

    config_choice(
        KernelArch
        ARCH
        "Architecture to use when building the kernel"
        "arm;KernelArchARM;ARCH_ARM;KernelSel4ArchAarch32 OR KernelSel4ArchAarch64 OR KernelSel4ArchArmHyp"
        "riscv;KernelArchRiscV;ARCH_RISCV;KernelSel4ArchRiscV32 OR KernelSel4ArchRiscV64"
        "x86;KernelArchX86;ARCH_X86;KernelSel4ArchX86_64 OR KernelSel4ArchIA32"
    )

    # The following config options are legacy and can be removed if they
    # aren't used anywhere anymore.
    if(KernelArchARM)
        config_set(KernelArmSel4Arch ARM_SEL4_ARCH "${KernelSel4Arch}")
    elseif(KernelArchRiscV)
        config_set(KernelRiscVSel4Arch RISCV_SEL4_ARCH "${KernelSel4Arch}")
    elseif(KernelArchX86)
        config_set(KernelX86Sel4Arch X86_SEL4_ARCH "${KernelSel4Arch}")
    endif()

    # arm-hyp masquerades as an aarch32 build
    if(KernelSel4ArchArmHyp)
        config_set(KernelSel4ArmHypAarch32 ARCH_AARCH32 ON)
        set(KernelSel4ArchAarch32 ON CACHE INTERNAL "" FORCE)
    else()
        config_set(KernelSel4ArmHypAarch32 ARCH_AARCH32 OFF)
    endif()

    # Set kernel mode options
    if(KernelSel4ArchAarch32 OR KernelSel4ArchArmHyp OR KernelSel4ArchRiscV32 OR KernelSel4ArchIA32)
        set_kernel_32()
    elseif(KernelSel4ArchAarch64 OR KernelSel4ArchRiscV64 OR KernelSel4ArchX86_64)
        set_kernel_64()
    endif()

endmacro()

# Register a platform's config options to be set if it is selected.
# Additionally, the kernel_platforms variable can be used as a record of all
# platforms that can be built once the platform configuration files have been
# processed.
# name: name of the platform, KernelPlatform will be set to this.
# config1: the CMake configuration name. Such as KernelPlatImx7.
# config2: the C header file config name without CONFIG_. Such as PLAT_IMX7_SABRE.
# enable_test: A CMake boolean formula that allows the option to be selected.
#     e.g. "KernelSel4ArchAarch32", or "KernelSel4ArchX86_64 OR KernelSel4ArchIA32"
macro(declare_platform name config1 config2 enable_test)
    list(APPEND kernel_platforms "${name}\;${config1}\;${config2}\;${enable_test}")
    if("${KernelPlatform}" STREQUAL ${name})
        set(${config1} ON CACHE INTERNAL "" FORCE)
        # Write KernelPlatform into the cache in case it is only a local variable
        set(KernelPlatform ${KernelPlatform} CACHE STRING "")
    else()
        set(${config1} OFF CACHE INTERNAL "" FORCE)
    endif()
endmacro()

function(declare_default_headers)
    cmake_parse_arguments(
        PARSE_ARGV
        0
        CONFIGURE
        ""
        "TIMER_FREQUENCY;MAX_IRQ;PLIC_MAX_NUM_INT;INTERRUPT_CONTROLLER;TIMER;SMMU"
        ""
    )
    # calculate the irq cnode size based on MAX_IRQ
    if("${KernelArch}" STREQUAL "riscv")
        set(MAX_IRQ "${CONFIGURE_PLIC_MAX_NUM_INT}")
        math(EXPR MAX_IRQ "${MAX_IRQ} + 2")
    else()
        set(MAX_IRQ "${CONFIGURE_MAX_IRQ}")
    endif()
    set(BITS "0")
    set(MAX "${MAX_IRQ}")
    while(MAX GREATER "0")
        math(EXPR BITS "${BITS} + 1")
        math(EXPR MAX "${MAX} >> 1")
    endwhile()
    math(EXPR SLOTS "1 << ${BITS}")
    if("${SLOTS}" LESS "${MAX_IRQ}")
        math(EXPR BITS "${BITS} + 1")
    endif()
    set(CONFIGURE_IRQ_SLOT_BITS "${BITS}")
    # variables parsed by the above will be prepended with CONFIGURE_, so pipe them
    # straight to configure_file
    configure_file(
        src/arch/${KernelArch}/platform_gen.h.in
        ${CMAKE_CURRENT_BINARY_DIR}/gen_headers/plat/platform_gen.h @ONLY
    )
    include_directories(include/plat/default)
endfunction()

# For all of the common variables we set a default value here if they haven't
# been set by a platform.
foreach(
    var
    IN
    ITEMS
    KernelArmCortexA7
    KernelArmCortexA8
    KernelArmCortexA9
    KernelArmCortexA15
    KernelArmCortexA53
    KernelArmCortexA57
    KernelArm1136JF_S
    KernelArchArmV6
    KernelArchArmV7a
    KernelArchArmV7ve
    KernelArchArmV8a
    KernelArmPASizeBits40
    KernelArmPASizeBits44
)
    unset(${var} CACHE)
    set(${var} OFF)
endforeach()
unset(KernelArmMach CACHE)
unset(KernelArmMachFeatureModifiers CACHE)
unset(KernelArmCPU CACHE)
unset(KernelArmArmV CACHE)

file(GLOB result ${CMAKE_CURRENT_LIST_DIR}/src/plat/*/config.cmake)
list(SORT result)

foreach(file ${result})
    include("${file}")
endforeach()

config_choice(KernelPlatform PLAT "Select the platform" ${kernel_platforms})

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
config_set(KernelArchArmV8a ARCH_ARM_V8A "${KernelArchArmV8a}")
config_set(KernelArmPASizeBits40 ARM_PA_SIZE_BITS_40 "${KernelArmPASizeBits40}")
config_set(KernelArmPASizeBits44 ARM_PA_SIZE_BITS_44 "${KernelArmPASizeBits44}")

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
    config_set(KernelArmCacheLineSizeBits L1_CACHE_LINE_SIZE_BITS "6")
elseif(KernelArmCortexA8)
    set(KernelArmCPU "cortex-a8" CACHE INTERNAL "")
    config_set(KernelArmCacheLineSizeBits L1_CACHE_LINE_SIZE_BITS "6")
elseif(KernelArmCortexA9)
    set(KernelArmCPU "cortex-a9" CACHE INTERNAL "")
    config_set(KernelArmCacheLineSizeBits L1_CACHE_LINE_SIZE_BITS "5")
elseif(KernelArmCortexA15)
    set(KernelArmCPU "cortex-a15" CACHE INTERNAL "")
    config_set(KernelArmCacheLineSizeBits L1_CACHE_LINE_SIZE_BITS "6")
elseif(KernelArmCortexA53)
    set(KernelArmCPU "cortex-a53" CACHE INTERNAL "")
    config_set(KernelArmCacheLineSizeBits L1_CACHE_LINE_SIZE_BITS "6")
elseif(KernelArmCortexA57)
    set(KernelArmCPU "cortex-a57" CACHE INTERNAL "")
    config_set(KernelArmCacheLineSizeBits L1_CACHE_LINE_SIZE_BITS "6")
elseif(KernelArm1136JF_S)
    set(KernelArmCPU "arm1136jf-s" CACHE INTERNAL "")
    config_set(KernelArmCacheLineSizeBits L1_CACHE_LINE_SIZE_BITS "5")
endif()
if(KernelArchARM)
    config_set(KernelArmMach ARM_MACH "${KernelArmMach}")
endif()

# Set defaults for common variables
set(KernelHaveFPU OFF)
set(KernelSetTLSBaseSelf OFF)

include(src/arch/arm/config.cmake)
include(src/arch/riscv/config.cmake)
include(src/arch/x86/config.cmake)

include(include/32/mode/config.cmake)
include(include/64/mode/config.cmake)
include(src/config.cmake)

if(DEFINED KernelDTSList)
    set(KernelDTSIntermediate "${CMAKE_CURRENT_BINARY_DIR}/kernel.dts")
    set(
        KernelDTBPath "${CMAKE_CURRENT_BINARY_DIR}/kernel.dtb"
        CACHE INTERNAL "Location of kernel DTB file"
    )
    set(compatibility_outfile "${CMAKE_CURRENT_BINARY_DIR}/kernel_compat.txt")
    set(device_dest "${CMAKE_CURRENT_BINARY_DIR}/gen_headers/plat/machine/devices_gen.h")
    set(
        platform_yaml "${CMAKE_CURRENT_BINARY_DIR}/gen_headers/plat/machine/platform_gen.yaml"
        CACHE INTERNAL "Location of platform YAML description"
    )
    set(config_file "${CMAKE_CURRENT_SOURCE_DIR}/tools/hardware.yml")
    set(config_schema "${CMAKE_CURRENT_SOURCE_DIR}/tools/hardware_schema.yml")

    find_program(DTC_TOOL dtc)
    if("${DTC_TOOL}" STREQUAL "DTC_TOOL-NOTFOUND")
        message(FATAL_ERROR "Cannot find 'dtc' program.")
    endif()

    # Generate final DTS based on Linux DTS + seL4 overlay[s]
    foreach(entry ${KernelDTSList})
        get_absolute_source_or_binary(dts_tmp ${entry})
        list(APPEND dts_list "${dts_tmp}")
    endforeach()

    check_outfile_stale(regen ${KernelDTBPath} dts_list ${CMAKE_CURRENT_BINARY_DIR}/dts.cmd)
    if(regen)
        file(REMOVE "${KernelDTSIntermediate}")
        foreach(entry ${dts_list})
            file(READ ${entry} CONTENTS)
            file(APPEND "${KernelDTSIntermediate}" "${CONTENTS}")
        endforeach()
        # Compile DTS to DTB
        execute_process(
            COMMAND
                ${DTC_TOOL} -q -I dts -O dtb -o ${KernelDTBPath} ${KernelDTSIntermediate}
        )
    endif()

    set(deps ${KernelDTBPath} ${config_file} ${config_schema} ${HARDWARE_GEN_PATH})
    check_outfile_stale(regen ${device_dest} deps ${CMAKE_CURRENT_BINARY_DIR}/gen_header.cmd)
    if(regen)
        # Generate devices_gen header based on DTB
        message(STATUS "${device_dest} is out of date. Regenerating...")
        file(MAKE_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}/gen_headers/plat/machine/")
        execute_process(
            COMMAND
                ${PYTHON} "${HARDWARE_GEN_PATH}" --dtb "${KernelDTBPath}" --compatibility-strings
                "${compatibility_outfile}" --output "${device_dest}" --config "${config_file}"
                --schema "${config_schema}" --yaml "${platform_yaml}" --arch "${KernelArch}"
            INPUT_FILE /dev/stdin
            OUTPUT_FILE /dev/stdout
            ERROR_FILE /dev/stderr
            RESULT_VARIABLE error
        )
        if(error)
            message(FATAL_ERROR "Failed to generate: ${device_dest}")
        endif()
    endif()
    file(READ "${compatibility_outfile}" compatibility_strings)

    # Mark all file dependencies as CMake rerun dependencies.
    set(cmake_deps ${deps} ${KernelDTSIntermediate} ${KernelDTSList} ${compatibility_outfile})
    set_property(
        DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}"
        APPEND
        PROPERTY CMAKE_CONFIGURE_DEPENDS ${cmake_deps}
    )

    include(src/drivers/config.cmake)
endif()

# Enshrine common variables in the config
config_set(KernelHaveFPU HAVE_FPU "${KernelHaveFPU}")

# System parameters
config_string(
    KernelRootCNodeSizeBits ROOT_CNODE_SIZE_BITS "Root CNode Size (2^n slots) \
    The acceptable range is 8-27 and 7-26, for 32-bit and 64-bit respectively. \
    The root CNode needs at least enough space to contain up to BI_CAP_DYN_START."
    DEFAULT 12
    UNQUOTE
)

config_string(KernelTimerTickMS TIMER_TICK_MS "Timer tick period in milliseconds" DEFAULT 2 UNQUOTE)
config_string(
    KernelTimeSlice TIME_SLICE "Number of timer ticks until a thread is preempted."
    DEFAULT 5
    UNQUOTE
)
config_string(
    KernelRetypeFanOutLimit RETYPE_FAN_OUT_LIMIT
    "Maximum number of objects that can be created in a single Retype() invocation."
    DEFAULT 256
    UNQUOTE
)
config_string(
    KernelMaxNumWorkUnitsPerPreemption MAX_NUM_WORK_UNITS_PER_PREEMPTION
    "Maximum number of work units (delete/revoke iterations) until the kernel checks for\
    pending interrupts (and preempts the currently running syscall if interrupts are pending)."
    DEFAULT 100
    UNQUOTE
)
config_string(
    KernelResetChunkBits RESET_CHUNK_BITS
    "Maximum size in bits of chunks of memory to zero before checking a preemption point."
    DEFAULT 8
    UNQUOTE
)
config_string(
    KernelMaxNumBootinfoUntypedCaps MAX_NUM_BOOTINFO_UNTYPED_CAPS
    "Max number of bootinfo untyped caps"
    DEFAULT 230
    UNQUOTE
)
config_option(KernelFastpath FASTPATH "Enable IPC fastpath" DEFAULT ON)

config_string(
    KernelNumDomains NUM_DOMAINS "The number of scheduler domains in the system"
    DEFAULT 1
    UNQUOTE
)

find_file(
    KernelDomainSchedule default_domain.c
    PATHS src/config
    CMAKE_FIND_ROOT_PATH_BOTH
    DOC "A C file providing the symbols ksDomSchedule and ksDomeScheudleLength \
        to be linked with the kernel as a scheduling configuration."
)

config_string(
    KernelNumPriorities NUM_PRIORITIES "The number of priority levels per domain. Valid range 1-256"
    DEFAULT 256
    UNQUOTE
)

config_string(
    KernelMaxNumNodes MAX_NUM_NODES "Max number of CPU cores to boot"
    DEFAULT 1
    DEPENDS "${KernelNumDomains} EQUAL 1;NOT KernelArchRiscV"
    UNQUOTE
)

config_string(
    KernelStackBits KERNEL_STACK_BITS
    "This describes the log2 size of the kernel stack. Great care should be taken as\
    there is no guard below the stack so setting this too small will cause random\
    memory corruption"
    DEFAULT 12
    UNQUOTE
)

config_string(
    KernelFPUMaxRestoresSinceSwitch FPU_MAX_RESTORES_SINCE_SWITCH
    "This option is a heuristic to attempt to detect when the FPU is no longer in use,\
    allowing the kernel to save the FPU state out so that the FPU does not have to be\
    enabled/disabled every thread swith. Every time we restore a thread and there is\
    active FPU state, we increment this setting and if it exceeds this threshold we\
    switch to the NULL state."
    DEFAULT 64
    DEPENDS "KernelHaveFPU" UNDEF_DISABLED
    UNQUOTE
)

config_option(
    KernelVerificationBuild VERIFICATION_BUILD
    "When enabled this configuration option prevents the usage of any other options that\
    would compromise the verification story of the kernel. Enabling this option does NOT\
    imply you are using a verified kernel."
    DEFAULT ON
)

config_option(
    KernelDebugBuild DEBUG_BUILD "Enable debug facilities (symbols and assertions) in the kernel"
    DEFAULT ON
    DEPENDS "NOT KernelVerificationBuild"
    DEFAULT_DISABLED OFF
)

config_option(
    HardwareDebugAPI HARDWARE_DEBUG_API
    "Builds the kernel with support for a userspace debug API, which can \
    allows userspace processes to set breakpoints, watchpoints and to \
    single-step through thread execution."
    DEFAULT OFF
    DEPENDS "NOT KernelVerificationBuild"
)
config_option(
    KernelPrinting PRINTING
    "Allow the kernel to print out messages to the serial console during bootup and execution."
    DEFAULT "${KernelDebugBuild}"
    DEPENDS "NOT KernelVerificationBuild"
    DEFAULT_DISABLED OFF
)
config_choice(
    KernelBenchmarks
    KERNEL_BENCHMARK
    "Enable benchamrks including logging and tracing info. \
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
config_string(
    KernelMaxNumTracePoints MAX_NUM_TRACE_POINTS
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

config_option(
    KernelIRQReporting IRQ_REPORTING
    "seL4 does not properly check for and handle spurious interrupts. This can result \
    in unnecessary output from the kernel during debug builds. If you are CERTAIN these \
    messages are benign then use this config to turn them off."
    DEFAULT ON
    DEPENDS "KernelPrinting"
    DEFAULT_DISABLED OFF
)
config_option(
    KernelColourPrinting COLOUR_PRINTING
    "In debug mode, seL4 prints diagnostic messages to its serial output describing, \
    e.g., the cause of system call errors. This setting determines whether ANSI escape \
    codes are applied to colour code these error messages. You may wish to disable this \
    setting if your serial output is redirected to a file or pipe."
    DEFAULT ON
    DEPENDS "KernelPrinting"
    DEFAULT_DISABLED OFF
)
config_string(
    KernelUserStackTraceLength USER_STACK_TRACE_LENGTH
    "On a double fault the kernel can try and print out the users stack to aid \
    debugging. This option determines how many words of stack should be printed."
    DEFAULT 16
    DEPENDS "KernelPrinting" DEFAULT_DISABLED 0
    UNQUOTE
)

config_choice(
    KernelOptimisation
    KERNEL_OPT_LEVEL
    "Select the kernel optimisation level"
    "-O2;KerenlOptimisationO2;KERNEL_OPT_LEVEL_O2"
    "-Os;KerenlOptimisationOS;KERNEL_OPT_LEVEL_OS"
    "-O0;KerenlOptimisationO0;KERNEL_OPT_LEVEL_O0"
    "-O1;KerenlOptimisationO1;KERNEL_OPT_LEVEL_O1"
    "-O3;KerenlOptimisationO3;KERNEL_OPT_LEVEL_O3"
)

config_option(
    KernelFWholeProgram KERNEL_FWHOLE_PROGRAM
    "Enable -fwhole-program when linking kernel. This should work modulo gcc bugs, which \
    are not uncommon with -fwhole-program. Consider this feature experimental!"
    DEFAULT OFF
)

config_option(
    KernelDangerousCodeInjection DANGEROUS_CODE_INJECTION
    "Adds a system call that allows users to specify code to be run in kernel mode. \
    Useful for profiling."
    DEFAULT OFF
    DEPENDS
        "NOT KernelARMHypervisorSupport;NOT KernelVerificationBuild;NOT KernelPlatformHikey;NOT KernelSkimWindow"
)

config_option(
    KernelDebugDisablePrefetchers DEBUG_DISABLE_PREFETCHERS
    "On ia32 platforms, this option disables the L2 hardware prefetcher, the L2 adjacent \
    cache line prefetcher, the DCU prefetcher and the DCU IP prefetcher. On the cortex \
    a53 this disables the L1 Data prefetcher."
    DEFAULT OFF
    DEPENDS "KernelArchX86 OR KernelPlatformHikey"
)

# Builds the kernel with support for an invocation to set the TLS_BASE
# of the currently running thread without a capability.
config_set(KernelSetTLSBaseSelf SET_TLS_BASE_SELF ${KernelSetTLSBaseSelf})

add_config_library(kernel "${configure_string}")
