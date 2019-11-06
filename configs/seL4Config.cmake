#
# Copyright 2019, Data61
# Commonwealth Scientific and Industrial Research Organisation (CSIRO)
# ABN 41 687 119 230.
#
# This software may be distributed and modified according to the terms of
# the GNU General Public License version 2. Note that NO WARRANTY is provided.
# See "LICENSE_GPLv2.txt" for details.
#
# @TAG(DATA61_GPL)
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
# Create and set all of the Kernel config options that can
# be derived from the seL4 arch which is one of the following:
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

# helper macro to unify messages printed output
# Usage example: print_message_multiple_options_helper("architectures" aarch32)
macro(print_message_multiple_options_helper str_type default_str)
    message(STATUS "platform ${KernelPlatform} supports multiple ${str_type}, none was given")
    message(STATUS "  defaulting to: ${default_str}")
endmacro()

# each sub platform configuration is a string of the form
#    "[sub platfrom name],[CMake variable],[C define]"
# Having "," as separator avoids clashing with native CMakes lists, which are
# simply strings using ";" as item separator. To process items of our ","
# separated list, we can simply replace the "," by ";" and it becomes a CMake
# list.
# This macro takes optional arguments of variable name, which are populated
# with the config items.
macro(get_sub_platform_items sub_plat_cfg)
    string(REPLACE "," ";" __tmp_plat_cfg_list ${sub_plat_cfg})
    if(${ARGC} GREATER 1) # get sub platform name
        list(GET __tmp_plat_cfg_list 0 ${ARGV1})
        if(${ARGC} GREATER 2) # get sub platform CMake variale
            list(GET __tmp_plat_cfg_list 1 ${ARGV2})
            if(${ARGC} GREATER 3) # get sub platform C define
                list(GET __tmp_plat_cfg_list 2 ${ARGV3})
            endif()
        endif()
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
# optional arguments are a list of sub platforms of the form
#    "[sub platfrom name],[CMake variable],[C define]"
# for example "foo,KernelPlatformFoo,PLAT_FOO"
# The first sub platform in the list is used as default platform if no sub
# platform is specified.

macro(declare_platform name config1 config2 enable_test)

    list(APPEND kernel_platforms "${name}\;${config1}\;${config2}\;${enable_test}")

    unset(__var_cmake_kernel_plat)
    unset(__arch_c_define)

    if("${enable_test}" MATCHES "^(KernelArchARM|KernelSel4ArchAarch32|KernelSel4ArchAarch64|KernelSel4ArchArmHyp)(\ OR\ .*)?$")
        set(__var_cmake_kernel_plat "KernelARMPlatform")
        set(__arch_c_define "ARM_PLAT")
    elseif("${enable_test}" MATCHES "^(KernelArchRiscV|KernelSel4ArchRiscV32|KernelSel4ArchRiscV64)$")
        set(__var_cmake_kernel_plat "KernelRiscVPlatform")
        set(__arch_c_define "RISCV_PLAT")
    elseif("${enable_test}" MATCHES "^(KernelArchX86)$")
        set(__var_cmake_kernel_plat "KernelX86Platform")
        set(__arch_c_define "X86_PLAT")
    else()
        message(FATAL_ERROR "unsupported filter: ${enable_test}")
    endif()

    set(__all_sub_plat_cfg ${ARGN})
    foreach(__plat_cfg IN LISTS __all_sub_plat_cfg)
        get_sub_platform_items(${__plat_cfg} __sub_name __var_cmake_sub_plat)

        if("${KernelPlatform}" STREQUAL "${__sub_name}")
            message(STATUS "replacing KernelPlatform=${KernelPlatform} with")
            message(STATUS "  KernelPlatform=${name}, ${__var_cmake_kernel_plat}=${__sub_name}")

            set(${__var_cmake_sub_plat} ON CACHE INTERNAL "" FORCE)
            set(KernelPlatform ${name} CACHE STRING "" FORCE)
            set(${__var_cmake_kernel_plat} ${__sub_name} CACHE STRING "" FORCE)
        else()
            unset(${__var_cmake_sub_plat} CACHE)
        endif()
    endforeach()

    if("${KernelPlatform}" STREQUAL "${name}")

        set(${config1} ON CACHE INTERNAL "" FORCE)

        # Write KernelPlatform into the cache in case it is only a local variable
        set(KernelPlatform ${KernelPlatform} CACHE STRING "")

        # check platform is valid and fall back to default sub platform
        list(LENGTH __all_sub_plat_cfg __cnt)
        if(${__cnt} EQUAL 0)

            set(__plat_name "${name}")

        else()

            if("${${__var_cmake_kernel_plat}}" STREQUAL "")
                list(GET __all_sub_plat_cfg 0 __plat_cfg_default)
                get_sub_platform_items(${__plat_cfg_default} __sub_plat_name_default)
                print_message_multiple_options_helper("sub platforms" ${__sub_plat_name_default})
                set(${__var_cmake_kernel_plat} ${__sub_plat_name_default})
            endif()

            # check if the sub platform is valid.
            unset(__is_sub_plat_valid)
            foreach(__plat_cfg IN LISTS __all_sub_plat_cfg)
                get_sub_platform_items(${__plat_cfg} __plat_name __plat_var_cmake __plat_c_define)
                if(${${__var_cmake_kernel_plat}} STREQUAL "${__plat_name}")

                    set(__is_sub_plat_valid "1")

                    # Followig the example from abve, this is effectively
                    #   config_set(KernelPlatformFoo PLAT_FOO ON)
                    config_set(${__plat_var_cmake} ${__plat_c_define} ON)

                    break()
                endif()
            endforeach()

            if("${__is_sub_plat_valid}" STREQUAL "")
                message(FATAL_ERROR "${KernelPlatform} platform unknown: ${${__var_cmake_kernel_plat}}")
            endif()
        endif()

        # Followig the example from above, this is effectivelly
        #   config_set(KernelARMPlatform ARM_PLAT "foo")
        config_set(${__var_cmake_kernel_plat} ${__arch_c_define} "${__plat_name}")

    else()

        set(${config1} OFF CACHE INTERNAL "" FORCE)
        config_set(${config1} ${config2} OFF)

    endif()
endmacro()

# helper macro that prints a message that no architecture is specified and
# the default architecture will be used
# Usage example: fallback_declare_seL4_arch_default(aarch32)
macro(fallback_declare_seL4_arch_default default_arch)
    print_message_multiple_options_helper("architectures" ${default_arch})
    declare_seL4_arch(${default_arch})
endmacro()

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
        "TIMER_FREQUENCY;MAX_IRQ;NUM_PPI;PLIC_MAX_NUM_INT;INTERRUPT_CONTROLLER;TIMER;SMMU;CLK_SHIFT;CLK_MAGIC;KERNEL_WCET;TIMER_PRECISION"
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
    set(CONFIGURE_TIMER_PRECISION "${CONFIGURE_TIMER_PRECISION}" CACHE INTERNAL "")
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
config_set(KernelArmCortexA53 ARM_CORTEX_A53 "${KernelArmCortexA53}")
config_set(KernelArmCortexA57 ARM_CORTEX_A57 "${KernelArmCortexA57}")
config_set(KernelArm1136JF_S ARM1136JF_S "${KernelArm1136JF_S}")
config_set(KernelArchArmV6 ARCH_ARM_V6 "${KernelArchArmV6}")
config_set(KernelArchArmV7a ARCH_ARM_V7A "${KernelArchArmV7a}")
config_set(KernelArchArmV7ve ARCH_ARM_V7VE "${KernelArchArmV7ve}")
config_set(KernelArchArmV8a ARCH_ARM_V8A "${KernelArchArmV8a}")
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
