#
# Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
#
# SPDX-License-Identifier: GPL-2.0-only
#

#
# Architecture selection
#
set(configure_string "")
set(c_sources "")
set(asm_sources "")
set(bf_declarations "")

include(${CMAKE_CURRENT_LIST_DIR}/../tools/internal.cmake)
include(${CMAKE_CURRENT_LIST_DIR}/../tools/helpers.cmake)

# Create and set all of the Kernel config options that can be derived from the
# seL4 arch which is one of the following:
# aarch32, aarch64, arm_hyp, riscv32, riscv64, x86_64, ia32
# This macro is intended to be called from within a platform config.
macro(declare_seL4_arch sel4_arch)
    set(KernelSel4Arch "${sel4_arch}" CACHE STRING "" FORCE)
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

    if(KernelSel4ArchArmHyp)
        # arm-hyp is basically aarch32. This should be cleaned up and aligned
        # with other architectures, where hypervisor support is an additional
        # flag. The main blocker here is updating the verification flow.
        config_set(KernelSel4ArchAarch32 ARCH_AARCH32 ON)
    endif()

    config_choice(
        KernelArch
        ARCH
        "Architecture to use when building the kernel"
        "arm;KernelArchARM;ARCH_ARM;KernelSel4ArchAarch32 OR KernelSel4ArchAarch64"
        "riscv;KernelArchRiscV;ARCH_RISCV;KernelSel4ArchRiscV32 OR KernelSel4ArchRiscV64"
        "x86;KernelArchX86;ARCH_X86;KernelSel4ArchX86_64 OR KernelSel4ArchIA32"
    )

    # Set kernel mode options
    if(KernelSel4ArchAarch32 OR KernelSel4ArchRiscV32 OR KernelSel4ArchIA32)
        config_set(KernelWordSize WORD_SIZE 32)
        set(Kernel64 OFF CACHE INTERNAL "")
        set(Kernel32 ON CACHE INTERNAL "")
    elseif(KernelSel4ArchAarch64 OR KernelSel4ArchRiscV64 OR KernelSel4ArchX86_64)
        config_set(KernelWordSize WORD_SIZE 64)
        set(Kernel64 ON CACHE INTERNAL "")
        set(Kernel32 OFF CACHE INTERNAL "")
    endif()

endmacro()

# helper macro to unify messages printed output
# Usage example: print_message_multiple_options_helper("architectures" aarch32)
macro(print_message_multiple_options_helper str_type default_str)
    message(STATUS "platform ${KernelPlatform} supports multiple ${str_type}, none was given")
    message(STATUS "  defaulting to: ${default_str}")
endmacro()

# helper macro that prints a message that no sub platform is specified and
# the default sub platform will be used
# Usage example: check_platform_and_fallback_to_default(KernelARMPlatform "sabre")
macro(check_platform_and_fallback_to_default var_cmake_kernel_plat default_sub_plat)
    if("${${var_cmake_kernel_plat}}" STREQUAL "")
        print_message_multiple_options_helper("sub platforms" ${default_sub_plat})
        set(${var_cmake_kernel_plat} ${default_sub_plat})
    endif()
endmacro()

# helper macro that prints a message that no architecture is specified and
# the default architecture will be used
# Usage example: fallback_declare_seL4_arch_default(aarch32)
macro(fallback_declare_seL4_arch_default default_arch)
    print_message_multiple_options_helper("architectures" ${default_arch})
    declare_seL4_arch(${default_arch})
endmacro()

# helper function that replaces all '-' by '_' and capitalizes all chars
function(sanitize_str_for_var out_var str)
    string(REPLACE "-" "_" str "${str}")
    string(TOUPPER "${str}" str)
    set(${out_var} "${str}" PARENT_SCOPE)
endfunction()

# Register a platform's config options to be set if it is selected.
# Additionally, the kernel_platforms variable can be used as a record of all
# platforms that can be built once the platform configuration files have been
# processed.
#
# Parameters:
#
#   <name>
#     name of the platform, KernelPlatform will be set to this.
#
#   <arch_list>
#     list of architectures, default is first item in list
#
#   MACH <mach>
#     optional, use common SOC architecture

#   CAMKE_VAR <var>
#     optional, customize name of CMake variable for platform, the default name
#     is KernelPlatform_${name} with '-' replaced by '_'
#
#   C_DEFINE <PLAT_xxx>
#     optional, customize name of C code fine for platform, teh default name is
#     PLAT_NAME (name capitalize, '-' replaced by '_'). The C define will be
#     prefixed with "CONFIG_" eventually
#
#   NO_DEFAULT_DTS
#     optional, do not use default DTS. This is "tools/dts/${name}.dts" unless
#     BOARDS is specified, then it is "tools/dts/${board-name}.dts"
#
#   SOURCES <file1> <file2> ...
#     files to add to the build
#
#   BOARDS <tupel_list, element=<name>[,<cmake_var>[,<c-define>]]>
#     optional, list of boards, default to first item
function(declare_platform name arch_list)

    cmake_parse_arguments(
        PARSE_ARGV
        1
        PARAM
        "NO_DEFAULT_DTS" # options
        "CAMKE_VAR;PLAT;C_DEFINE;MACH" # one-value keywords
        "PLAT_CAMKE_VARS;SOURCES;BOARDS" # multi-value keywords
    )

    # Generating a CMake variable from the name requires replacing every "-" by
    # "_". That's all what is needed for the platform names currently in use,
    # new names may require additional sanitizing. We also capitalize all
    # letters to have the C define and the CMake variable aligned closely.
    #message(STATUS "${name}    (KernelPlatform='${KernelPlatform}')")
    sanitize_str_for_var(name_as_var "${name}")

    if(DEFINED PARAM_CAMKE_VAR)
        #message(STATUS "   CAMKE_VAR: KernelPlatform_${name_as_var} -> ${PARAM_CAMKE_VAR}")
    else()
        set(PARAM_CAMKE_VAR "KernelPlatform_${name_as_var}")
    endif()

    if(DEFINED PARAM_C_DEFINE)
        #message(STATUS "   C_DEFINE: PLAT_${name_as_var} -> ${PARAM_C_DEFINE}")
    else()
        set(PARAM_C_DEFINE "PLAT_${name_as_var}")
    endif()

    if(PARAM_NO_DEFAULT_DTS)
        #message(STATUS "   DTS: (none by default)")
    endif()

    # disable any CMake variables by default
    set(${PARAM_CAMKE_VAR} OFF CACHE INTERNAL "" FORCE)
    foreach(plat_cmake_var IN LISTS PLAT_CAMKE_VARS PARAM_CAMKE_VAR)
        unset(${plat_cmake_var} CACHE)
        set(${plat_cmake_var} OFF)
    endforeach()

    # Check that <arch_list> contains only valid seL4 architectures. The map
    # below specifies the relation between values the variable KernelSel4Arch
    # can have and the corresponding variable KernelSel4Archxxx.
    set(
        arch_mapping
           "ia32:KernelSel4ArchIA32"
         "x86_64:KernelSel4ArchX86_64"
        "aarch32:KernelSel4ArchAarch32"
        "arm_hyp:KernelSel4ArchArmHyp" # legacy hack, removed one day
        "aarch64:KernelSel4ArchAarch64"
        "riscv32:KernelSel4ArchRiscV32"
        "riscv64:KernelSel4ArchRiscV64"
    )

    set(filter "")
    foreach(a IN LISTS arch_list)
        if(NOT ";${arch_mapping};" MATCHES ";${a}:([^;]*);")
            message(FATAL_ERROR "KernelPlatform '${name}': unsupported architecture '${a}'")
        endif()
        list(APPEND filter "${CMAKE_MATCH_1}")
    endforeach()
    string(REPLACE ";" " OR " enable_test "${filter}")

    # now we have the basic platform parameters that the build system needs
    set(
        kernel_platforms
        "${kernel_platforms}"
        "${name},${PARAM_CAMKE_VAR},${PARAM_C_DEFINE},${enable_test}"
        PARENT_SCOPE)

    set(board_names "")
    if(DEFINED PARAM_BOARDS)

        foreach(board IN LISTS PARAM_BOARDS)

            string(REPLACE "," ";" board_descr "${board}")
            list(LENGTH board_descr cnt)
            list(GET board_descr 0 board_name)

            #message(STATUS "   board: ${board_name}")

            list(APPEND board_names "${board_name}")
            sanitize_str_for_var(board_name_as_var "${board_name}")
            set(board_cmake_var "KernelPlatform_${board_name_as_var}")
            set(board_c_define "PLAT_${board_name_as_var}")

            if(cnt GREATER 1)
                list(GET board_descr 1 board_cmake_var)
                #message(STATUS "   CAMKE_VAR: KernelPlatform_${board_name_as_var} -> ${board_cmake_var}")
                if(cnt GREATER 2)
                    list(GET board_descr 2 board_c_define)
                    #message(STATUS "   C_DEFINE: PLAT_${board_name_as_var} -> ${board_c_define}")
                endif()
            endif()

            # disable any CMake variables by default
            unset(${board_cmake_var} CACHE)
            set(${board_cmake_var} OFF)

        endforeach()

        if(name IN_LIST board_names)
            message(FATAL_ERROR "Platform name '${name}' can't be in list of board names")
        endif()

    endif()

    # if this is not the currently selected platform, then just ensure it's
    # disabled and we are done here
    set(arch_plat "${name}")
    if(KernelPlatform IN_LIST board_names)
        set(arch_plat "${KernelPlatform}")
        set(KernelPlatform "${name}")
    elseif(NOT KernelPlatform STREQUAL "${name}")
        #message(STATUS "   OFF")
        return()
    elseif(board_names)
        # first board is the default if nothing else is set
        list(GET board_names 0 defaut_board)
        set(arch_plat "${defaut_board}")
    endif()

    # this is the currently selected platform, setup defaults

    if(NOT KernelSel4Arch)
        message(STATUS "KernelPlatform '${KernelPlatform}': architectures not specified")
        message(STATUS "  options: ${arch_list}")
        list(GET arch_list 0 KernelSel4Arch)
        message(STATUS "  defaulting to '${KernelSel4Arch}'")
    elseif(NOT KernelSel4Arch IN_LIST arch_list)
        message(FATAL_ERROR "KernelPlatform '${KernelPlatform}': unsupported KernelSel4Arch '${KernelSel4Arch}'")
    endif()


    # ToDo: don't we have this anywhere already?
    set(list_arch_x86 "ia32;x86_64")
    set(list_arch_arm "aarch64;aarch32;arm_hyp")
    set(list_arch_riscv "riscv64;riscv32")

    set(KernelPlatform "${KernelPlatform}" CACHE STRING "")
    set(${PARAM_CAMKE_VAR} ON CACHE INTERNAL "" FORCE)
    declare_seL4_arch("${KernelSel4Arch}")

    if(DEFINED PARAM_MACH)
        if(KernelSel4Arch IN_LIST list_arch_arm)
            config_set(KernelArmMach MACH "${PARAM_MACH}")
        else()
            message(FATAL_ERROR "MACH not supported in this architecture")
        endif()
    endif()

    if(KernelSel4Arch IN_LIST list_arch_arm)

        if(KernelARMPlatform)

            # check that KernelARMPlatform is a valid board
            if((DEFINED PARAM_BOARDS) AND (NOT KernelARMPlatform IN_LIST board_names))
                message(FATAL_ERROR "unknown board '${KernelARMPlatform}'")
            endif()

            if(NOT "${KernelARMPlatform}" STREQUAL "${arch_plat}")
                message(STATUS "   change KernelARMPlatform: '${arch_plat}' -> '${KernelARMPlatform}'")
            else()
                message(STATUS "   KernelARMPlatform is already '${KernelARMPlatform}'")
            endif()

            set(arch_plat "${KernelARMPlatform}")

        endif()

        if(DEFINED PARAM_BOARDS)
            # if ${arch_plat} is ${name} then use first ${board_name}

            # we know here that PARAM_BOARDS contains ${arch_plat}
            foreach(board IN LISTS PARAM_BOARDS)
                string(REPLACE "," ";" board_descr "${board}")
                list(GET board_descr 0 board_name)
                if("${board_name}" STREQUAL "${arch_plat}")
                    sanitize_str_for_var(board_name_as_var "${board_name}")
                    set(board_cmake_var "KernelPlatform_${board_name_as_var}")
                    set(board_c_define "PLAT_${board_name_as_var}")
                    list(LENGTH board_descr cnt)
                    if(cnt GREATER 1)
                        list(GET board_descr 1 board_cmake_var)
                        if(cnt GREATER 2)
                            list(GET board_descr 2 board_c_define)
                        endif()
                    endif()

                    message(STATUS "   set board: '${board_cmake_var}', '${board_c_define}'")
                    config_set("${board_cmake_var}" "${board_c_define}" ON)
                    break()
                endif()
            endforeach()
        endif()

        config_set(KernelARMPlatform ARM_PLAT "${arch_plat}")
    endif()

    if(DEFINED PARAM_SOURCES)
        foreach(f IN LISTS PARAM_SOURCES)
        message(STATUS "   add: ${f}")
        endforeach()

        add_sources(CFILES "${PARAM_SOURCES}")
        set(c_sources "${c_sources}" PARENT_SCOPE)
        set(asm_sources "${asm_sources}" PARENT_SCOPE)
    endif()


    if(NOT PARAM_NO_DEFAULT_DTS)
        set(main_dts "tools/dts/${arch_plat}.dts")
        if(NOT EXISTS "${CMAKE_CURRENT_FUNCTION_LIST_DIR}/../${main_dts}")
            message(FATAL_ERROR "missing DTS: ${main_dts}")
        endif()
        list(APPEND KernelDTSList "${main_dts}")
        set(KernelDTSList "${KernelDTSList}" PARENT_SCOPE)
    endif()

    # ensure the parent sees all the changes that e.g. config_set() made
    set(configure_string "${configure_string}" PARENT_SCOPE)

    #message(STATUS "   ON")

endfunction()



unset(CONFIGURE_PLIC_MAX_NUM_INT CACHE)
unset(CONFIGURE_TIMER_FREQUENCY CACHE)
unset(CONFIGURE_MAX_IRQ CACHE)
unset(CONFIGURE_NUM_PPI CACHE)
unset(CONFIGURE_INTERRUPT_CONTROLLER CACHE)
unset(CONFIGURE_TIMER CACHE)
unset(CONFIGURE_SMMU CACHE)
unset(CONFIGURE_CLK_SHIFT CACHE)
unset(CONFIGURE_CLK_MAGIC CACHE)
unset(CONFIGURE_KERNEL_WCET CACHE)
unset(CONFIGURE_TIMER_PRECISION CACHE)
# CONFIGURE_MAX_CB and CONFIGURE_MAX_SID are related to the kernel SMMU on Arm.
unset(CONFIGURE_MAX_SID CACHE)
unset(CONFIGURE_MAX_CB CACHE)

# CLK_SHIFT and CLK_MAGIC are generated from tools/reciprocal.py
# based on the TIMER_CLK_HZ to simulate division.
# This could be moved to a cmake function
# in future to build the values on the first build. Note the calculation
# can take a long time though.
function(declare_default_headers)
    cmake_parse_arguments(
        PARSE_ARGV
        0
        CONFIGURE
        ""
        "TIMER_FREQUENCY;MAX_IRQ;NUM_PPI;PLIC_MAX_NUM_INT;INTERRUPT_CONTROLLER;TIMER;SMMU;CLK_SHIFT;CLK_MAGIC;KERNEL_WCET;TIMER_PRECISION;MAX_SID;MAX_CB"
        ""
    )
    set(CONFIGURE_TIMER_FREQUENCY "${CONFIGURE_TIMER_FREQUENCY}" CACHE INTERNAL "")
    set(CONFIGURE_MAX_IRQ "${CONFIGURE_MAX_IRQ}" CACHE INTERNAL "")
    set(CONFIGURE_NUM_PPI "${CONFIGURE_NUM_PPI}" CACHE INTERNAL "")
    set(CONFIGURE_PLIC_MAX_NUM_INT "${CONFIGURE_PLIC_MAX_NUM_INT}" CACHE INTERNAL "")
    set(CONFIGURE_INTERRUPT_CONTROLLER "${CONFIGURE_INTERRUPT_CONTROLLER}" CACHE INTERNAL "")
    set(CONFIGURE_TIMER "${CONFIGURE_TIMER}" CACHE INTERNAL "")
    set(CONFIGURE_SMMU "${CONFIGURE_SMMU}" CACHE INTERNAL "")
    set(CONFIGURE_CLK_SHIFT "${CONFIGURE_CLK_SHIFT}" CACHE INTERNAL "")
    set(CONFIGURE_CLK_MAGIC "${CONFIGURE_CLK_MAGIC}" CACHE INTERNAL "")
    set(CONFIGURE_KERNEL_WCET "${CONFIGURE_KERNEL_WCET}" CACHE INTERNAL "")
    if(DEFINED CONFIGURE_TIMER_PRECISION)
        set(CONFIGURE_TIMER_PRECISION "${CONFIGURE_TIMER_PRECISION}" CACHE INTERNAL "")
    endif()
    set(CONFIGURE_MAX_SID "${CONFIGURE_MAX_SID}" CACHE INTERNAL "")
    set(CONFIGURE_MAX_CB "${CONFIGURE_MAX_CB}" CACHE INTERNAL "")
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
    KernelArmCortexA35
    KernelArmCortexA53
    KernelArmCortexA55
    KernelArmCortexA57
    KernelArmCortexA72
    KernelArm1136JF_S
    KernelArchArmV6
    KernelArchArmV7a
    KernelArchArmV7ve
    KernelArchArmV8a
    KernelArmSMMU
    KernelAArch64SErrorIgnore
)
    unset(${var} CACHE)
    set(${var} OFF)
endforeach()
unset(KernelArmMach CACHE)
unset(KernelArmMachFeatureModifiers CACHE)
unset(KernelArmCPU CACHE)
unset(KernelArmArmV CACHE)

# Blacklist platforms without MCS support
unset(KernelPlatformSupportsMCS CACHE)
set(KernelPlatformSupportsMCS ON)

file(GLOB result ${CMAKE_CURRENT_LIST_DIR}/../src/plat/*/config.cmake)
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
config_set(KernelArmCortexA35 ARM_CORTEX_A35 "${KernelArmCortexA35}")
config_set(KernelArmCortexA53 ARM_CORTEX_A53 "${KernelArmCortexA53}")
config_set(KernelArmCortexA55 ARM_CORTEX_A55 "${KernelArmCortexA55}")
config_set(KernelArmCortexA57 ARM_CORTEX_A57 "${KernelArmCortexA57}")
config_set(KernelArmCortexA72 ARM_CORTEX_A72 "${KernelArmCortexA72}")
config_set(KernelArm1136JF_S ARM1136JF_S "${KernelArm1136JF_S}")
config_set(KernelArchArmV6 ARCH_ARM_V6 "${KernelArchArmV6}")
config_set(KernelArchArmV7a ARCH_ARM_V7A "${KernelArchArmV7a}")
config_set(KernelArchArmV7ve ARCH_ARM_V7VE "${KernelArchArmV7ve}")
config_set(KernelArchArmV8a ARCH_ARM_V8A "${KernelArchArmV8a}")
config_set(KernelArmSMMU ARM_SMMU "${KernelArmSMMU}")
config_set(KernelAArch64SErrorIgnore AARCH64_SERROR_IGNORE "${KernelAArch64SErrorIgnore}")
set(KernelPlatformSupportsMCS "${KernelPlatformSupportsMCS}" CACHE INTERNAL "" FORCE)

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
elseif(KernelArmCortexA35)
    set(KernelArmCPU "cortex-a35" CACHE INTERNAL "")
elseif(KernelArmCortexA53)
    set(KernelArmCPU "cortex-a53" CACHE INTERNAL "")
elseif(KernelArmCortexA55)
    set(KernelArmCPU "cortex-a55" CACHE INTERNAL "")
elseif(KernelArmCortexA57)
    set(KernelArmCPU "cortex-a57" CACHE INTERNAL "")
elseif(KernelArmCortexA72)
    set(KernelArmCPU "cortex-a72" CACHE INTERNAL "")
elseif(KernelArm1136JF_S)
    set(KernelArmCPU "arm1136jf-s" CACHE INTERNAL "")
endif()
if(KernelArchARM)
    config_set(KernelArmMach ARM_MACH "${KernelArmMach}")
endif()

set(config_configure_string ${configure_string} CACHE INTERNAL "")
set(config_c_sources "")
set(config_asm_sources "")
set(config_bf_declarations "")
foreach(file IN LISTS c_sources)
    string(
        REPLACE
            "${CMAKE_CURRENT_SOURCE_DIR}/"
            ""
            file
            "${file}"
    )
    list(APPEND config_c_sources "${file}")
endforeach()
foreach(file IN LISTS asm_sources)
    string(
        REPLACE
            "${CMAKE_CURRENT_SOURCE_DIR}/"
            ""
            file
            "${file}"
    )
    list(APPEND config_asm_sources "${file}")
endforeach()
foreach(file IN LISTS bf_declarations)
    string(
        REPLACE
            "${CMAKE_CURRENT_SOURCE_DIR}/"
            ""
            file
            "${file}"
    )
    list(APPEND config_bf_declarations "${file}")
endforeach()

set(config_c_sources ${config_c_sources} CACHE INTERNAL "")
set(config_asm_sources ${config_asm_sources} CACHE INTERNAL "")
set(config_bf_declarations ${config_bf_declarations} CACHE INTERNAL "")
set(config_KernelDTSList ${KernelDTSList} CACHE INTERNAL "")

if("${TRIPLE}" STREQUAL "")
    set(toolchain_file gcc.cmake)
else()
    set(toolchain_file llvm.cmake)
endif()
set(toolchain_outputfile "${CMAKE_BINARY_DIR}/${toolchain_file}")
if(
    ("${CMAKE_TOOLCHAIN_FILE}" STREQUAL "")
    OR ("${CMAKE_TOOLCHAIN_FILE}" STREQUAL "${toolchain_outputfile}")
)
    configure_file(
        "${CMAKE_CURRENT_LIST_DIR}/../${toolchain_file}" "${toolchain_outputfile}.temp" @ONLY
    )
    if(EXISTS "${toolchain_outputfile}")
        file(READ "${toolchain_outputfile}.temp" filea)
        file(READ "${toolchain_outputfile}" fileb)
        if(NOT "${filea}" STREQUAL "${fileb}")
            message(
                FATAL_ERROR
                    "Config changes have resulted in a different toolchain file. This is not supported"
            )
        endif()
    endif()
    file(RENAME "${toolchain_outputfile}.temp" "${toolchain_outputfile}")
    set(CMAKE_TOOLCHAIN_FILE "${toolchain_outputfile}" CACHE PATH "")
endif()
