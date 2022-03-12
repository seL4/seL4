#
# Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
#
# SPDX-License-Identifier: GPL-2.0-only
#

cmake_minimum_required(VERSION 3.7.2)

config_option(
    KernelIsMCS KERNEL_MCS "Use the MCS kernel configuration, which is not verified."
    DEFAULT OFF
)

# Error for unsupported MCS platforms
if(KernelIsMCS AND (NOT KernelPlatformSupportsMCS))
    message(
        FATAL_ERROR "KernelIsMCS selected, but platform: ${KernelPlatform} does not support it."
    )
endif()

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

# These options are now set in seL4Config.cmake
if(DEFINED CALLED_declare_default_headers)
    # calculate the irq cnode size based on MAX_NUM_IRQ
    if("${KernelArch}" STREQUAL "riscv")
        math(EXPR MAX_NUM_IRQ "${CONFIGURE_PLIC_MAX_NUM_INT} + 2")
    else()
        if(
            DEFINED KernelMaxNumNodes
            AND CONFIGURE_NUM_PPI GREATER "0"
            AND "${KernelArch}" STREQUAL "arm"
        )
            math(
                EXPR MAX_NUM_IRQ
                "(${KernelMaxNumNodes}-1)*${CONFIGURE_NUM_PPI} + ${CONFIGURE_MAX_IRQ}"
            )
        else()
            set(MAX_NUM_IRQ "${CONFIGURE_MAX_IRQ}")
        endif()
    endif()
    set(BITS "0")
    while(MAX_NUM_IRQ GREATER "0")
        math(EXPR BITS "${BITS} + 1")
        math(EXPR MAX_NUM_IRQ "${MAX_NUM_IRQ} >> 1")
    endwhile()
    set(CONFIGURE_IRQ_SLOT_BITS "${BITS}" CACHE INTERNAL "")
    if(NOT DEFINED CONFIGURE_TIMER_PRECISION)
        set(CONFIGURE_TIMER_PRECISION "0")
    endif()
    configure_file(
        src/arch/${KernelArch}/platform_gen.h.in
        ${CMAKE_CURRENT_BINARY_DIR}/gen_headers/plat/platform_gen.h @ONLY
    )
    include_directories(include/plat/default)
endif()

# Set defaults for common variables
set(KernelHaveFPU OFF)
set(KernelSetTLSBaseSelf OFF)

include(src/arch/${KernelArch}/config.cmake)
include(include/${KernelWordSize}/mode/config.cmake)
include(src/config.cmake)

if(DEFINED KernelDTSList AND (NOT "${KernelDTSList}" STREQUAL ""))
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
    set(
        KernelCustomDTSOverlay ""
        CACHE FILEPATH "Provide an additional overlay to append to the selected KernelPlatform's \
        device tree during build time"
    )
    if(NOT "${KernelCustomDTSOverlay}" STREQUAL "")
        if(NOT EXISTS ${KernelCustomDTSOverlay})
            message(FATAL_ERROR "Can't open external overlay file '${KernelCustomDTSOverlay}'!")
        endif()
        list(APPEND KernelDTSList "${KernelCustomDTSOverlay}")
        message(STATUS "Using ${KernelCustomDTSOverlay} overlay")
    endif()

    find_program(DTC_TOOL dtc)
    if("${DTC_TOOL}" STREQUAL "DTC_TOOL-NOTFOUND")
        message(FATAL_ERROR "Cannot find 'dtc' program.")
    endif()
    find_program(STAT_TOOL stat)
    if("${STAT_TOOL}" STREQUAL "STAT_TOOL-NOTFOUND")
        message(FATAL_ERROR "Cannot find 'stat' program.")
    endif()
    mark_as_advanced(DTC_TOOL STAT_TOOL)
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
            RESULT_VARIABLE error
        )
        if(error)
            message(FATAL_ERROR "Failed to compile DTS to DTB: ${KernelDTBPath}")
        endif()
        # CMAKE_HOST_APPLE is a built-in CMake variable
        if(CMAKE_HOST_APPLE)
            set(STAT_ARGS "-f%z")
        else()
            set(STAT_ARGS "-c '%s'")
        endif()
        # Track the size of the DTB for downstream tools
        execute_process(
            COMMAND ${STAT_TOOL} ${STAT_ARGS} ${KernelDTBPath}
            OUTPUT_VARIABLE KernelDTBSize
            OUTPUT_STRIP_TRAILING_WHITESPACE
            RESULT_VARIABLE error
        )
        if(error)
            message(FATAL_ERROR "Failed to determine KernelDTBSize: ${KernelDTBPath}")
        endif()
        string(
            REPLACE
                "\'"
                ""
                KernelDTBSize
                ${KernelDTBSize}
        )
        set(KernelDTBSize "${KernelDTBSize}" CACHE INTERNAL "Size of DTB blob, in bytes")
    endif()

    set(deps ${KernelDTBPath} ${config_file} ${config_schema} ${HARDWARE_GEN_PATH})
    check_outfile_stale(regen ${device_dest} deps ${CMAKE_CURRENT_BINARY_DIR}/gen_header.cmd)
    if(regen)
        # Generate devices_gen header based on DTB
        message(STATUS "${device_dest} is out of date. Regenerating from DTB...")
        file(MAKE_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}/gen_headers/plat/machine/")
        execute_process(
            COMMAND
                ${PYTHON3} "${HARDWARE_GEN_PATH}" --dtb "${KernelDTBPath}" --compat-strings
                --compat-strings-out "${compatibility_outfile}" --c-header --header-out
                "${device_dest}" --hardware-config "${config_file}" --hardware-schema
                "${config_schema}" --yaml --yaml-out "${platform_yaml}" --sel4arch
                "${KernelSel4Arch}" --addrspace-max "${KernelPaddrUserTop}"
            RESULT_VARIABLE error
        )
        if(error)
            message(FATAL_ERROR "Failed to generate from DTB: ${device_dest}")
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
config_set(KernelPaddrUserTop PADDR_USER_DEVICE_TOP "${KernelPaddrUserTop}")

# System parameters
config_string(
    KernelRootCNodeSizeBits ROOT_CNODE_SIZE_BITS "Root CNode Size (2^n slots) \
    The acceptable range is 8-27 and 7-26, for 32-bit and 64-bit respectively. \
    The root CNode needs at least enough space to contain up to BI_CAP_DYN_START."
    DEFAULT 12
    UNQUOTE
)

config_string(
    KernelTimerTickMS TIMER_TICK_MS "Timer tick period in milliseconds"
    DEFAULT 2
    UNQUOTE
    DEPENDS "NOT KernelIsMCS" UNDEF_DISABLED
)
config_string(
    KernelTimeSlice TIME_SLICE "Number of timer ticks until a thread is preempted."
    DEFAULT 5
    UNQUOTE
    DEPENDS "NOT KernelIsMCS" UNDEF_DISABLED
)
config_string(
    KernelBootThreadTimeSlice BOOT_THREAD_TIME_SLICE
    "Number of milliseconds until the boot thread is preempted."
    DEFAULT 5
    UNQUOTE
    DEPENDS "KernelIsMCS" UNDEF_DISABLED
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
if(SEL4_CONFIG_DEFAULT_ADVANCED)
    mark_as_advanced(KernelDomainSchedule)
endif()

config_string(
    KernelNumPriorities NUM_PRIORITIES "The number of priority levels per domain. Valid range 1-256"
    DEFAULT 256
    UNQUOTE
)

config_string(
    KernelMaxNumNodes MAX_NUM_NODES "Max number of CPU cores to boot"
    DEFAULT 1
    DEPENDS "${KernelNumDomains} EQUAL 1"
    UNQUOTE
)

# Set CONFIG_ENABLE_SMP_SUPPORT as an alias of CONFIG_MAX_NUM_NODES > 1
if(KernelMaxNumNodes GREATER 1)
    config_set(KernelEnableSMPSupport ENABLE_SMP_SUPPORT ON)
else()
    config_set(KernelEnableSMPSupport ENABLE_SMP_SUPPORT OFF)
endif()

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
    DEPENDS "NOT KernelVerificationBuild;NOT KernelHardwareDebugAPIUnsupported"
)
config_option(
    KernelPrinting PRINTING
    "Allow the kernel to print out messages to the serial console during bootup and execution."
    DEFAULT "${KernelDebugBuild}"
    DEPENDS "NOT KernelVerificationBuild"
    DEFAULT_DISABLED OFF
)

config_option(
    KernelInvocationReportErrorIPC KERNEL_INVOCATION_REPORT_ERROR_IPC
    "Allows the kernel to write the userError to the IPC buffer"
    DEFAULT OFF
    DEPENDS "KernelPrinting"
    DEFAULT_DISABLED OFF
)

config_choice(
    KernelBenchmarks
    KERNEL_BENCHMARK
    "Enable benchmarks including logging and tracing info. \
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

# Reflect the existence of kernel Log buffer
if(KernelBenchmarksTrackKernelEntries OR KernelBenchmarksTracepoints)
    config_set(KernelLogBuffer KERNEL_LOG_BUFFER ON)
else()
    config_set(KernelLogBuffer KERNEL_LOG_BUFFER OFF)
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
    "-O2;KernelOptimisationO2;KERNEL_OPT_LEVEL_O2"
    "-Os;KernelOptimisationOS;KERNEL_OPT_LEVEL_OS"
    "-O0;KernelOptimisationO0;KERNEL_OPT_LEVEL_O0"
    "-O1;KernelOptimisationO1;KERNEL_OPT_LEVEL_O1"
    "-O3;KernelOptimisationO3;KERNEL_OPT_LEVEL_O3"
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

config_string(
    KernelWcetScale KERNEL_WCET_SCALE
    "Multiplier to scale kernel WCET estimate by: the kernel WCET estimate  \
     is used to ensure a thread has enough budget to get in and out of the  \
     kernel. When running in a simulator the WCET estimate, which is tuned  \
     for hardware, may not be sufficient. This option provides a hacky knob \
     that can be fiddled with when running inside a simulator."
    DEFAULT 1
    UNQUOTE
    DEPENDS "KernelIsMCS" UNDEF_DISABLED
)

config_string(
    KernelStaticMaxPeriodUs KERNEL_STATIC_MAX_PERIOD_US
    "Specifies a static maximum to which scheduling context can have \
    either its period or budget configured."
    DEFAULT 0
    UNQUOTE
    DEPENDS "KernelIsMCS" UNDEF_DISABLED
)

config_option(
    KernelClz32 CLZ_32 "Define a __clzsi2 function to count leading zeros for uint32_t arguments. \
                        Only needed on platforms which lack a builtin instruction."
    DEFAULT OFF
)

config_option(
    KernelClz64 CLZ_64 "Define a __clzdi2 function to count leading zeros for uint64_t arguments. \
                        Only needed on platforms which lack a builtin instruction."
    DEFAULT OFF
)

config_option(
    KernelCtz32 CTZ_32 "Define a __ctzsi2 function to count trailing zeros for uint32_t arguments. \
                        Only needed on platforms which lack a builtin instruction."
    DEFAULT OFF
)

config_option(
    KernelCtz64 CTZ_64 "Define a __ctzdi2 function to count trailing zeros for uint64_t arguments. \
                        Only needed on platforms which lack a builtin instruction."
    DEFAULT OFF
)

config_option(
    KernelClzNoBuiltin CLZ_NO_BUILTIN
    "Expose implementations of clzl and clzll to verification by avoiding the use \
     of __builtin_clzl and __builtin_clzll."
    DEFAULT OFF
)

config_option(
    KernelCtzNoBuiltin CTZ_NO_BUILTIN
    "Expose implementations of ctzl and ctzll to verification by avoiding the use \
     of __builtin_ctzl and __builtin_ctzll."
    DEFAULT OFF
)

add_config_library(kernel "${configure_string}")
