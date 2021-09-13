#
# Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
#
# SPDX-License-Identifier: GPL-2.0-only
#

cmake_minimum_required(VERSION 3.7.2)

# File for helpers that are very specific to the kernel

function(gen_invocation_header)
    cmake_parse_arguments(PARSE_ARGV 0 "GEN" "LIBSEL4;ARCH;SEL4ARCH" "OUTPUT;XML" "")
    if(NOT "${GEN_UNPARSED_ARGUMENTS}" STREQUAL "")
        message(FATAL_ERROR "Unknown arguments to gen_invocation_header: ${GEN_UNPARSED_ARGUMENTS}")
    endif()
    # Ensure only one of arch or sel4arch
    if(GEN_ARCH AND GEN_SEL4ARCH)
        message(FATAL_ERROR "Can only specify one of ARCH or SEL4ARCH")
    endif()
    # OUTPUT and XML are required
    if(("${GEN_OUTPUT}" STREQUAL "") OR ("${GEN_XML}" STREQUAL ""))
        message(FATAL_ERROR "OUTPUT and XML must both be specified")
    endif()
    set(arch_setting "")
    if(GEN_ARCH)
        set(arch_setting "--arch")
    elseif(GEN_SEL4ARCH)
        set(arch_setting "--sel4_arch")
    endif()
    set(libsel4_setting "")
    if(GEN_LIBSEL4)
        set(libsel4_setting "--libsel4")
    endif()
    # Turn the input xml into an absolute path as we build commands that may be
    # be run with a working directory that is not the current source directory
    get_absolute_source_or_binary(xml_absolute "${GEN_XML}")
    add_custom_command(
        OUTPUT "${GEN_OUTPUT}"
        COMMAND rm -f "${GEN_OUTPUT}"
        COMMAND
            "${PYTHON3}" "${INVOCATION_ID_GEN_PATH}"
            --xml "${xml_absolute}" ${libsel4_setting} ${arch_setting}
            --dest "${GEN_OUTPUT}"
        DEPENDS "${xml_absolute}" "${INVOCATION_ID_GEN_PATH}"
        COMMENT "Generate invocation header ${GEN_OUTPUT}"
    )
endfunction(gen_invocation_header)

# Adds files to the global sources list, but only if the supplied dependencies are met.
# A dependency lists can be specified with DEP and CFILES are added to c_sources whilst
# ASMFILES are added to asm_sources. An PREFIX can be given as path to prefix to each
# C and ASM file given
function(add_sources)
    cmake_parse_arguments(PARSE_ARGV 0 "ADD" "" "DEP;PREFIX" "CFILES;ASMFILES")
    if(NOT "${ADD_UNPARSED_ARGUMENTS}" STREQUAL "")
        message(FATAL_ERROR "Unknown arguments to add_c_sources: ${ADD_UNPARSED_ARGUMENTS}")
    endif()
    # Need to prefix files with the CMAKE_CURRENT_SOURCE_DIR as we use these
    # in custom commands whose working directory is not the source directory
    # Also need to ensure that if an additional prefix wasn't specified by the
    # caller, that we don't add an additional /, as this will screw up file sorting
    if(NOT "${ADD_PREFIX}" STREQUAL "")
        set(ADD_PREFIX "${ADD_PREFIX}/")
    endif()
    set(ADD_PREFIX "${CMAKE_CURRENT_SOURCE_DIR}/${ADD_PREFIX}")
    foreach(file IN LISTS ADD_CFILES)
        list(APPEND new_c_sources "${ADD_PREFIX}${file}")
    endforeach()
    foreach(file IN LISTS ADD_ASMFILES)
        list(APPEND new_asm_sources "${ADD_PREFIX}${file}")
    endforeach()
    list_append_if(c_sources "${ADD_DEP}" ${new_c_sources})
    list_append_if(asm_sources "${ADD_DEP}" ${new_asm_sources})
endfunction(add_sources)

# If the dependencies list in the 'dep' argument is true then a bf file is added
# to the bf_declarations list. The format of file+prefix+path is used in order to
# separate where the bf file is located in the source, versus where it will get
# generated.
function(add_bf_source_old dep file prefix path)
    list_append_if(
        bf_declarations "${dep}" "${CMAKE_CURRENT_SOURCE_DIR}/${prefix}/${path}/${file}:${path}"
    )
endfunction(add_bf_source_old)
