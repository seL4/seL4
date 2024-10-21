#
# Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
# Copyright 2024, Capabilities Limited
# CHERI support contributed by Capabilities Limited was developed by Hesham Almatary
#
# SPDX-License-Identifier: GPL-2.0-only
#

cmake_minimum_required(VERSION 3.8.2)

# Wrapper function around find_file that generates a fatal error if it isn't found
# Is equivalent to find_file except that it adds CMAKE_CURRENT_SOURCE_DIR as a path and sets
# CMAKE_FIND_ROOT_PATH_BOTH
function(RequireFile config_name file_name)
    find_file(
        ${config_name} "${file_name}"
        PATHS "${CMAKE_CURRENT_SOURCE_DIR}"
        CMAKE_FIND_ROOT_PATH_BOTH ${ARGV}
    )
    if("${${config_name}}" STREQUAL "${config_name}-NOTFOUND")
        message(FATAL_ERROR "Failed to find required file ${file_name}")
    endif()
    mark_as_advanced(FORCE ${config_name})
endfunction(RequireFile)

# Helper function for converting a filename to an absolute path. It first converts to
# an absolute path based in the current source directory, and if the results in a file
# that doesn't exist it returns an absolute path based from the binary directory
# This file check is done at generation time and is considered safe as source files
# should not be being added as part of the build step (except into the build directory)
function(get_absolute_source_or_binary output input)
    get_filename_component(test "${input}" ABSOLUTE BASE_DIR "${CMAKE_CURRENT_SOURCE_DIR}")
    if(NOT EXISTS "${test}")
        get_filename_component(test "${input}" ABSOLUTE BASE_DIR "${CMAKE_CURRENT_BINARY_DIR}")
    endif()
    set("${output}" "${test}" PARENT_SCOPE)
endfunction(get_absolute_source_or_binary)

function(get_absolute_list_source_or_binary output input)
    get_filename_component(test "${input}" ABSOLUTE BASE_DIR "${CMAKE_CURRENT_LIST_DIR}")
    if(NOT EXISTS "${test}")
        get_absolute_source_or_binary(test ${input})
    endif()
    set("${output}" "${test}" PARENT_SCOPE)
endfunction()

# Generates a custom command that preprocesses an input file into an output file
# Uses the current compilation settings as well as any EXTRA_FLAGS provided. Can also
# be given any EXTRA_DEPS to depend upon
# A target with the name `output_target` will be generated to create a target based dependency
# for the output file
# Output and input files will be converted to absolute paths based on the following rules
#  * Output is assumed to be in CMAKE_CURRENT_BINARY_DIR
#  * Input is assumed to be in CMAKE_CURRENT_SOURCE_DIR if it resolves to a file that exists
#    otherwise it is assumed to be in CMAKE_CURRENT_BINARY_DIR
function(cppfile output output_target input)
    cmake_parse_arguments(PARSE_ARGV 3 "CPP" "" "EXACT_NAME" "EXTRA_DEPS;EXTRA_FLAGS")
    if(NOT "${CPP_UNPARSED_ARGUMENTS}" STREQUAL "")
        message(FATAL_ERROR "Unknown arguments to cppfile: ${CPP_UNPARSED_ARGUMENTS}")
    endif()
    get_absolute_source_or_binary(input "${input}")
    set(file_copy_name "${output_target}_temp.c")
    # If EXACT_NAME then we copy the input file to the name given by the caller. Otherwise
    # generate a rule for copying the input file to a default name.
    if(CPP_EXACT_NAME)
        set(file_copy_name ${CPP_EXACT_NAME})
    endif()
    add_custom_command(
        OUTPUT ${file_copy_name}
        COMMAND
            ${CMAKE_COMMAND} -E copy ${input} ${CMAKE_CURRENT_BINARY_DIR}/${file_copy_name}
        COMMENT "Creating C input file for preprocessor"
        DEPENDS ${CPP_EXTRA_DEPS} ${input}
    )
    add_custom_target(${output_target}_copy_in DEPENDS ${file_copy_name})
    # Now generate an object library to persuade cmake to just do compilation and not try
    # and link our 'object' files
    add_library(${output_target}_temp_lib OBJECT ${file_copy_name})
    add_dependencies(${output_target}_temp_lib ${output_target}_copy_in)
    # Give the preprecess flag
    target_compile_options(${output_target}_temp_lib PRIVATE -E)
    # Give any other flags from the user
    target_compile_options(${output_target}_temp_lib PRIVATE ${CPP_EXTRA_FLAGS})
    # Now copy from the random name cmake gave our object file into the one desired by the user
    add_custom_command(
        OUTPUT ${output}
        COMMAND
            ${CMAKE_COMMAND} -E copy $<TARGET_OBJECTS:${output_target}_temp_lib> ${output}
        DEPENDS ${output_target}_temp_lib $<TARGET_OBJECTS:${output_target}_temp_lib>
    )
    add_custom_target(${output_target} DEPENDS ${output})
endfunction(cppfile)

# Function to generate a custom command to process a bitfield file. The input
# (pbf_path) is either a .bf file or, if you used pre-processor directives, a
# pre-processed .bf file. As this invokes a python tool that places a file
# in the current working directory a unqiue 'work_dir' needs to be provided
# for this command to execute in
# This function is not intended to be used directly, rather one of its wrappers
# that is specialized to generate a specific kind of output should be used
# These wrappers work by passing the additional 'args' that get passed on to
# the bitfield generator
function(GenBFCommand args target_name pbf_path pbf_target deps)
    # Since we're going to change the working directory first convert any paths to absolute
    get_filename_component(
        target_name_absolute
        "${target_name}"
        ABSOLUTE
        BASE_DIR
        "${CMAKE_CURRENT_BINARY_DIR}"
    )
    get_absolute_source_or_binary(pbf_path_absolute "${pbf_path}")
    add_custom_command(
        OUTPUT "${target_name_absolute}"
        COMMAND
            "${PYTHON3}" "${BF_GEN_PATH}" "${args}" "${pbf_path_absolute}" "${target_name_absolute}"
        DEPENDS
            "${BF_GEN_PATH}"
            "${pbf_path_absolute}"
            "${pbf_target}"
            ${deps}
        COMMENT "Generating from ${pbf_path}" COMMAND_EXPAND_LISTS
        VERBATIM
    )
endfunction(GenBFCommand)

# Wrapper function for generating both a target and command to process a bitfield file
function(GenBFTarget args target_name target_file pbf_path pbf_target deps)
    GenBFCommand("${args}" "${target_file}" "${pbf_path}" "${pbf_target}" "${deps}")
    add_custom_target(${target_name} DEPENDS "${target_file}")
endfunction(GenBFTarget)

# Wrapper around GenBFTarget for generating a C header file out of a bitfield specification
# environment is empty for kernel generation and "libsel4" for generating non kernel headers
# prunes is an optional list of files that will be passed as --prune options to the bitfield
# generator
function(GenHBFTarget environment target_name target_file pbf_path pbf_target prunes deps orig_file)
    set(args "")
    if(NOT "${environment}" STREQUAL "")
        list(APPEND args --environment "${environment}")
    endif()

    if(HaveCheri)
        if(KernelArmMorello)
            list(APPEND args --cheri-arch "morello")
        elseif(KernelArchCheriRiscv)
            list(APPEND args --cheri-arch "rv${KernelWordSize}xcheri")
        endif()
    endif()

    foreach(prune IN LISTS prunes)
        get_absolute_source_or_binary(prune_absolute "${prune}")
        list(APPEND args "--prune" "${prune_absolute}")
    endforeach()
    list(APPEND args --from_file "${orig_file}")
    list(APPEND deps ${prunes})
    GenBFTarget("${args}" "${target_name}" "${target_file}" "${pbf_path}" "${pbf_target}" "${deps}")
endfunction(GenHBFTarget)

# Wrapper for generating different kinds of .thy files from bitfield specifications
function(GenThyBFTarget args target_name target_file pbf_path pbf_target prunes deps)
    get_filename_component(cspec_dir "${CSPEC_DIR}" ABSOLUTE BASE_DIR)
    list(APPEND args --cspec-dir "${cspec_dir}")
    if(SKIP_MODIFIES)
        list(APPEND args "--skip_modifies")
    endif()
    foreach(prune IN LISTS prunes)
        list(APPEND args "--prune" "${prune}")
    endforeach()
    GenBFTarget("${args}" "${target_name}" "${target_file}" "${pbf_path}" "${pbf_target}" "${deps}")
endfunction(GenThyBFTarget)

# Generate hol definitions from a bitfield specification
function(GenDefsBFTarget target_name target_file pbf_path pbf_target prunes deps)
    set(args "")
    list(APPEND args --hol_defs)
    GenThyBFTarget(
        "${args}"
        "${target_name}"
        "${target_file}"
        "${pbf_path}"
        "${pbf_target}"
        "${prunes}"
        "${deps}"
    )
endfunction(GenDefsBFTarget)

# Generate proofs from a bitfield specification
function(GenProofsBFTarget target_name target_file pbf_path pbf_target prunes deps)
    set(args "")
    # Get an absolute path to cspec_dir so that the final theory file is portable
    list(
        APPEND
            args
            --hol_proofs
            --umm_types
            "${UMM_TYPES}"
    )
    if(SORRY_BITFIELD_PROOFS)
        list(APPEND args "--sorry_lemmas")
    endif()
    list(
        APPEND
            args
            "--toplevel;$<JOIN:$<TARGET_PROPERTY:kernel_config_target,TOPLEVELTYPES>,;--toplevel;>"
    )
    list(APPEND deps "${UMM_TYPES}")
    GenThyBFTarget(
        "${args}"
        "${target_name}"
        "${target_file}"
        "${pbf_path}"
        "${pbf_target}"
        "${prunes}"
        "${deps}"
    )
endfunction(GenProofsBFTarget)

macro(cfg_str_add_enabled cfg_str name var)
    cfg_str_add_entry(${cfg_str} ${name} "true" "${var}=${${var}}")
endmacro()

macro(cfg_str_add_disabled cfg_str name)
    cfg_str_add_entry(${cfg_str} ${name} "false" "")
endmacro()

macro(cfg_str_add_string cfg_str name value)
    cfg_str_add_entry(${cfg_str} ${name} "\"${value}\"" "")
endmacro()

macro(cfg_str_add_entry cfg_str name value comment)
    set(cfg_str_entry "${name}: ${value}")
    if(NOT "${comment}" STREQUAL "")
        string(APPEND cfg_str_entry " # ${comment}")
    endif()
    string(APPEND ${cfg_str} "${cfg_str_entry}\n")
endmacro()

# config_option(cmake_option_name c_config_name doc DEFAULT default [DEPENDS deps] [DEFAULT_DISABLE default_disabled])
# Defines a toggleable configuration option that will be present in the cache and the
# cmake-gui
#  optionname is the name of the cache variable that can be used directly in cmake scripts
#   to get the value of the option
#  configname is the name (prefixed with CONFIG_) that will appear in generated
#   C configuration headers
#  DEFAULT is the default value of the config that it should initially be set to
#  doc Help string to explain the option in the cmake-gui
# An additional DEPENDS arguments may be passed, which is a list of conditions to evaluate and if true,
#  the option will exist. If the option doesn't exist it will be set to DEFAULT_DISABLED, or if
#  that wasn't provided then just DEFAULT
# If the option is true it adds to the global configure_string variable (see add_config_library)
function(config_option optionname configname doc)
    cmake_parse_arguments(PARSE_ARGV 3 "CONFIG" "" "DEPENDS;DEFAULT_DISABLED;DEFAULT" "")
    if(NOT "${CONFIG_UNPARSED_ARGUMENTS}" STREQUAL "")
        message(FATAL_ERROR "Unknown arguments to config_option")
    endif()
    if("${CONFIG_DEFAULT_DISABLED}" STREQUAL "")
        set(CONFIG_DEFAULT_DISABLED "${CONFIG_DEFAULT}")
    endif()
    set(valid ON)
    if(NOT "${CONFIG_DEPENDS}" STREQUAL "")
        # Check the passed in dependencies. This loop and logic is inspired by the
        # actual cmake_dependent_option code
        foreach(test ${CONFIG_DEPENDS})
            string(
                REGEX
                REPLACE
                    " +"
                    ";"
                    test
                    "${test}"
            )
            if(NOT (${test}))
                set(valid OFF)
                break()
            endif()
        endforeach()
    endif()
    if(valid)
        # Check for an existing value, and set the option to that, otherwise use the default
        # Also reset the default if we switched from disabled to enabled
        if((DEFINED ${optionname}) AND (NOT DEFINED ${optionname}_DISABLED))
            set(${optionname} "${${optionname}}" CACHE BOOL "${doc}" FORCE)
        else()
            set(${optionname} "${CONFIG_DEFAULT}" CACHE BOOL "${doc}" FORCE)
            unset(${optionname}_DISABLED CACHE)
        endif()
        # This is a directory scope setting used to allow or prevent config options
        # from appearing in the cmake config GUI
        if(SEL4_CONFIG_DEFAULT_ADVANCED)
            mark_as_advanced(${optionname})
        endif()
    else()
        set(${optionname} "${CONFIG_DEFAULT_DISABLED}" CACHE INTERNAL "${doc}" FORCE)
        set(${optionname}_DISABLED TRUE CACHE INTERNAL "" FORCE)
    endif()
    set(local_config_string "${configure_string}")
    if(${optionname})
        cfg_str_add_enabled(local_config_string ${configname} ${optionname})
    else()
        cfg_str_add_disabled(local_config_string ${configname})
    endif()
    set(configure_string "${local_config_string}" PARENT_SCOPE)
endfunction(config_option)

# Set a configuration option to a particular value. This value will not appear in
# the cmake-gui, but will produce an internal cmake cache variable and generated
# configuration headers.
macro(config_set optionname configname value)
    set(${optionname} "${value}" CACHE INTERNAL "" FORCE)
    if("${value}" STREQUAL "OFF")
        cfg_str_add_disabled(configure_string ${configname})
    else()
        if("${value}" STREQUAL "ON")
            cfg_str_add_enabled(configure_string ${configname} ${optionname})
        else()
            # we have to quote ${value} here because it could be empty
            cfg_str_add_string(configure_string ${configname} "${value}")
        endif()
    endif()
endmacro(config_set)

# config_cmake_string(cmake_option_name c_config_name doc DEFAULT default [DEPENDS dep]
#   [DEFAULT_DISABLED default_disabled] [UNDEF_DISABLED] [QUOTE])
# Defines a configuration option that is a user configurable string. Most parameters
# are the same as config_option
# UNQUOTE if specified says this is something with more semantics like a number or identifier
#  and should not be quoted in the output
# [UNDEF_DISABLED] can be specified to explicitly disable generation of any output value when
#  the configuration dependencies are unmet
# Adds to the global configure_string variable (see add_config_library)
function(config_string optionname configname doc)
    cmake_parse_arguments(
        PARSE_ARGV
        3
        "CONFIG"
        "UNQUOTE;UNDEF_DISABLED"
        "DEPENDS;DEFAULT_DISABLED;DEFAULT"
        ""
    )
    if(NOT "${CONFIG_UNPARSED_ARGUMENTS}" STREQUAL "")
        message(FATAL_ERROR "Unknown arguments to config_option: ${CONFIG_UNPARSED_ARGUMENTS}")
    endif()
    if("${CONFIG_DEFAULT}" STREQUAL "")
        message(FATAL_ERROR "No default specified for ${config_option}")
    endif()
    if("${CONFIG_DEFAULT_DISABLED}" STREQUAL "")
        set(CONFIG_DEFAULT_DISABLED "${CONFIG_DEFAULT}")
    endif()
    set(valid ON)
    set(local_config_string "${configure_string}")
    if(NOT "${CONFIG_DEPENDS}" STREQUAL "")
        # Check the passed in dependencies. This loop and logic is inspired by the
        # actual cmake_dependent_option code
        foreach(test ${CONFIG_DEPENDS})
            string(
                REGEX
                REPLACE
                    " +"
                    ";"
                    test
                    "${test}"
            )
            if(NOT (${test}))
                set(valid OFF)
                break()
            endif()
        endforeach()
    endif()
    set(cfg_tag_option "")
    if(valid)
        # See if we transitioned from disabled to enabled. We do this by having an
        # _UNAVAILABLE variable. We want to ensure that if the option previously had
        # unmet conditions that we reset its value to 'default'. This is needed
        # because whilst the option had unmet conditions it still potentially had
        # a value in the form of the optional disabled_value
        set(force "")
        if(${optionname}_UNAVAILABLE)
            set(force "FORCE")
            unset(${optionname}_UNAVAILABLE CACHE)
        endif()
        set(${optionname} "${CONFIG_DEFAULT}" CACHE STRING "${doc}" ${force})
        set(cfg_tag_option ${optionname})
        # This is a directory scope setting used to allow or prevent config options
        # from appearing in the cmake config GUI
        if(SEL4_CONFIG_DEFAULT_ADVANCED)
            mark_as_advanced(${optionname})
        endif()
    else()
        if(CONFIG_UNDEF_DISABLED)
            unset(${optionname} CACHE)
        else()
            # Forcively change the value to its disabled_value
            set(${optionname} "${CONFIG_DEFAULT_DISABLED}" CACHE INTERNAL "" FORCE)
            set(cfg_tag_option ${optionname})
        endif()
        # Sset _UNAVAILABLE so we can detect when the option because enabled again
        set(${optionname}_UNAVAILABLE ON CACHE INTERNAL "" FORCE)
    endif()
    if(cfg_tag_option)
        if(CONFIG_UNQUOTE)
            set(quote "")
        else()
            set(quote "@quote@")
        endif()
        cfg_str_add_string(local_config_string ${configname} "${quote}@${cfg_tag_option}@${quote}")
    endif()
    set(configure_string "${local_config_string}" PARENT_SCOPE)
endfunction(config_string)

# Defines a multi choice / select configuration option
#  optionname is the name of the cache variable that can be used directly in cmake scripts
#   to get the value of the option
#  configname is the name (prefixed with CONFIG_) that will appear in generated
#   C configuration headers and is set to the string of the selected config
#  doc Help string to explain the option in the cmake-gui
# Then any number of additional arguments may be supplied each describing one of the potential
# configuration choices. Each additional argument is a list of (option_value, option_cache,
# option_config, [condition]...)
#  option_value is the string that represents this option. this is what the user will see
#   in the cmake-gui and what configname will get defined to if this option is selected
#  option_cache is like optionname and is set to ON when this option is selected and OFF
#   if it is not
#  condition may be repeated as many times and all conditions must be true for this choice
#   to appear in the list
# If no valid choices are given (either because none are given or the ones that were given
# did not have their conditions met) then this option will be disabled and not appear in
# the cmake-gui
# Adds to the global configure_string variable (see add_config_library)
function(config_choice optionname configname doc)
    # Cannot use ARGN because each argument itself is a list
    math(EXPR limit "${ARGC} - 1")
    set(local_config_string "${configure_string}")
    # force_default represents whether we need to force a new value or not. We would need
    # to force a new value for example if we detect that the current selected choice is
    # no longer (due to conditions) a valid choice
    set(force_default "")
    # Used to track the first time we see a valid enabled choice. The first valid choice
    # becomes the default and if we never find a valid choice then we know to disable this config
    set(first ON)
    # This tracks whether or not the current selected choice is one of the ones that we
    # have been passed. If we fail to find the currently selected choice then, similar to
    # if the current choice is invalid to do an unment condition, we must switch to some
    # valid default
    set(found_current OFF)
    foreach(i RANGE 3 ${limit})
        set(option "${ARGV${i}}")
        # Extract the constant parts of the choice information and just leave any
        # conditional information
        list(GET option 0 option_value)
        list(GET option 1 option_cache)
        list(GET option 2 option_config)
        list(
            REMOVE_AT
                option
                0
                1
                2
        )
        # Construct a list of all of our options
        list(APPEND all_strings "${option_value}")
        # By default we assume is valid, we may change our mind after checking dependencies
        # (if there are any). This loop is again based off the one in cmake_dependent_option
        set(valid ON)
        foreach(truth IN LISTS option)
            string(
                REGEX
                REPLACE
                    " +"
                    ";"
                    truth
                    "${truth}"
            )
            if(NOT (${truth}))
                # This choice isn't valid due to unmet conditions so we must check if we have
                # currently selected this choice. If so trigger the force_default
                if("${${optionname}}" STREQUAL "${option_value}")
                    set(force_default "FORCE")
                endif()
                set(valid OFF)
            endif()
        endforeach()
        if(valid)
            # Is a valid option, add to the strings list
            list(APPEND strings "${option_value}")
            if(first)
                set(first OFF)
                set(first_cache "${option_cache}")
                set(first_config "${option_config}")
                # Use the first valid option we find as the default. This default is will be
                # used if there is no current value, or for some reason we need to override
                # the current value (see force_default above)
                set(default "${option_value}")
            endif()
            # Check if this option is the one that is currently set
            if("${${optionname}}" STREQUAL "${option_value}")
                set(${option_cache} ON CACHE INTERNAL "" FORCE)
                cfg_str_add_enabled(local_config_string ${option_config} ${option_cache})
                set(found_current ON)
            else()
                set(${option_cache} OFF CACHE INTERNAL "" FORCE)
                cfg_str_add_disabled(local_config_string ${option_config})
            endif()
        else()
            # Remove this config as it's not valid
            unset(${option_cache} CACHE)
        endif()
    endforeach()
    if(NOT found_current)
        # Currently selected option wasn't found so reset to a default that we know is valid
        set(force_default "FORCE")
    endif()
    if(first)
        # None of the choices were valid. Remove this option so its not visible
        unset(${optionname} CACHE)
    else()
        cfg_str_add_string(local_config_string ${configname} "@${optionname}@")
        set(configure_string "${local_config_string}" PARENT_SCOPE)
        set(${optionname} "${default}" CACHE STRING "${doc}" ${force_default})
        # This is a directory scope setting used to allow or prevent config options
        # from appearing in the cmake config GUI
        if(SEL4_CONFIG_DEFAULT_ADVANCED)
            mark_as_advanced(${optionname})
        endif()
        set_property(CACHE ${optionname} PROPERTY STRINGS ${strings})
        if(NOT found_current)
            # The option is actually enabled, but we didn't enable the correct
            # choice earlier, since we didn't know we were going to revert to
            # the default. So add the option setting here
            set(${first_cache} ON CACHE INTERNAL "" FORCE)
            cfg_str_add_enabled(local_config_string ${first_config} ${first_cache})
        endif()
    endif()
    # Save all possible options to an internal value.  This is to allow enumerating the options elsewhere.
    # We create a new variable because cmake doesn't support arbitrary properties on cache variables.
    set(${optionname}_all_strings ${all_strings} CACHE INTERNAL "" FORCE)
    set(configure_string "${local_config_string}" PARENT_SCOPE)
endfunction(config_choice)

# Defines a target for a 'configuration' library, which generates a header based
# upon current state of cache/variables and a provided template string. Additionally
# the generated library gets added to a known global list of 'configuration' libraries
# This list can be used if someone wants all the configurations
# Whilst this function takes an explicit configure_template, generally this will always
# be '${configure_string}' as that is the global variable automatically appended to
# by the config_ helper macros and functions above
# This generates a  library that can be linked against with
# target_link_library(<target> ${prefix}_Config)
# Which will allow you to do #include <${prefix}/gen_config.h>
function(add_config_library prefix configure_template)
    set(config_dir "${CMAKE_CURRENT_BINARY_DIR}/gen_config")
    set(config_yaml_file "${config_dir}/${prefix}/gen_config.yaml")
    set(config_header_file "${config_dir}/${prefix}/gen_config.h")
    set(config_json_file "${config_dir}/${prefix}/gen_config.json")

    set(quote "\"")
    string(CONFIGURE "${configure_template}" config_yaml_contents ESCAPE_QUOTES @ONLY)
    # An empty YAML file corresponds the scalar `null`, but we want an empty map in this case.
    if("${config_yaml_contents}" STREQUAL "")
        set(config_yaml_contents "{}")
    endif()
    file(WRITE "${config_yaml_file}" "${config_yaml_contents}")

    execute_process(
        COMMAND
            "${PYTHON3}" "${CONFIG_GEN_PATH}" "${config_yaml_file}" --write-c
            "${config_header_file}" --write-json "${config_json_file}"
        RESULT_VARIABLE error
    )
    if(error)
        message(FATAL_ERROR "Failed to generate header: ${config_yaml_file}")
    endif()

    add_custom_target(${prefix}_Gen DEPENDS "${config_header_file}")
    add_library(${prefix}_Config INTERFACE)
    target_include_directories(${prefix}_Config INTERFACE "${config_dir}")
    add_dependencies(${prefix}_Config ${prefix}_Gen ${config_header_file})
    set_property(GLOBAL APPEND PROPERTY CONFIG_LIBRARIES "${prefix}")
    # Set a property on the library that is a list of the files we generated. This
    # allows custom build commands to easily get a file dependency list so they can
    # 'depend' upon this target easily
    set_property(TARGET ${prefix}_Gen APPEND PROPERTY GENERATED_FILES ${config_header_file})
endfunction(add_config_library)

macro(get_generated_files output target)
    get_property(${output} TARGET ${target} PROPERTY GENERATED_FILES)
endmacro(get_generated_files)

# This rule tries to emulate an 'autoconf' header. autoconf generated headers
# were previously used as configuration, so this rule provides a way for previous
# applications and libraries to build without modification. The config_list
# is a list of 'prefix' values that have been passed to add_config_library
# This generates a library with ${targetname} that when linked against
# will allow code to simply #include <autoconf.h>
function(generate_autoconf targetname config_list)
    set(link_list "")
    set(gen_list "")
    set(config_header_contents "\n#pragma once\n\n")
    foreach(config IN LISTS config_list)
        list(APPEND link_list "${config}_Config")
        get_generated_files(gens ${config}_Gen)
        list(APPEND gen_list ${gens})
        string(APPEND config_header_contents "#include <${config}/gen_config.h>\n")
    endforeach()
    set(config_dir "${CMAKE_CURRENT_BINARY_DIR}/autoconf")
    set(config_file "${config_dir}/autoconf.h")

    file(GENERATE OUTPUT "${config_file}" CONTENT "${config_header_contents}")
    add_custom_target(${targetname}_Gen DEPENDS "${config_file}" ${gen_list})
    add_library(${targetname} INTERFACE)
    target_link_libraries(${targetname} INTERFACE ${link_list})
    target_include_directories(${targetname} INTERFACE "${config_dir}")
    add_dependencies(${targetname} ${targetname}_Gen ${config_file} ${gen_list})
    # Set our GENERATED_FILES property to include the GENERATED_FILES of all of our input
    # configurations, as well as the files we generated
    set_property(
        TARGET ${targetname}_Gen
        APPEND
        PROPERTY GENERATED_FILES "${config_file}" ${gen_list}
    )
endfunction(generate_autoconf)

# Macro that allows for appending to a specified list only if all the supplied conditions are true
macro(list_append_if list dep)
    set(list_append_local_list ${${list}})
    set(list_append_valid ON)
    foreach(truth IN ITEMS ${dep})
        string(
            REGEX
            REPLACE
                " +"
                ";"
                truth
                "${truth}"
        )
        if(NOT (${truth}))
            set(list_append_valid OFF)
            break()
        endif()
    endforeach()
    if(list_append_valid)
        list(APPEND list_append_local_list ${ARGN})
    endif()
    set(${list} ${list_append_local_list} PARENT_SCOPE)
endmacro(list_append_if)

# Checks if a file is older than its dependencies
# Will set `stale` to TRUE if outfile doesn't exist,
# or if outfile is older than any file in `deps_list`.
# Will also set `stale` to TRUE if the arguments given to this macro
# change compared to the previous invocation.
# stale: A variable to overwrite with TRUE or FALSE
# outfile: A value that is a valid file path
# deps_list: A variable that holds a list of file paths
# arg_cache: A variable that holds a file to store arguments to
# e.g:
#  set(dts_list "filea" "fileb" "filec")
#  set(KernelDTBPath "${CMAKE_CURRENT_BINARY_DIR}/kernel.dtb")
#  check_outfile_stale(regen ${KernelDTBPath} dts_list ${CMAKE_CURRENT_BINARY_DIR}/dts.cmd
#  if (regen)
#    regen_file(${KernelDTBPath})
#  endif()
#
# The above call will set regen to TRUE if the file referred
# to by KernelDTBPath doesn't exist, or is older than any files
# in KernelDTSIntermediate or if regen, ${KernelDTBPath} and dts_list resolve to different files.
macro(check_outfile_stale stale outfile deps_list arg_cache)
    set(_outfile_command "${stale} ${outfile} ${${deps_list}}")
    if(NOT EXISTS "${arg_cache}")
        set(_prev_command "")
    else()
        file(READ "${arg_cache}" _prev_command)
    endif()
    if(NOT "${_outfile_command}" STREQUAL "${_prev_command}")
        set(${stale} TRUE)
    else()
        set(${stale} FALSE)
    endif()
    if(EXISTS ${outfile} AND NOT ${stale})
        set(${stale} FALSE)
        foreach(dep IN LISTS ${deps_list})
            if("${dep}" IS_NEWER_THAN "${outfile}")
                set(${stale} TRUE)
                break()
            endif()
        endforeach()
    else()
        set(${stale} TRUE)
    endif()
    if(${stale})
        file(WRITE "${arg_cache}" "${_outfile_command}")
    endif()
endmacro()

# This macro only works when cmake is invoked with -P (script mode) on a kernel
# verified configuration. The result is configuring and building a verified kernel.
# CMAKE_ARGC and CMAKE_ARGV# contain command line argument information.
# It runs the following commands to produce kernel.elf and kernel_all_pp.c:
# cmake -G Ninja ${args} -C ${CMAKE_ARGV2} ${CMAKE_CURRENT_LIST_DIR}/..
# ninja kernel.elf
# ninja kernel_all_pp_wrapper
macro(cmake_script_build_kernel)
    if(NOT "${CMAKE_ARGC}" STREQUAL "")
        set(args "")
        foreach(i RANGE 3 ${CMAKE_ARGC})
            if("${CMAKE_ARGV${i}}" STREQUAL "FORCE")
                # Consume arg and force reinit of build dir by deleting CMakeCache.txt
                file(REMOVE CMakeCache.txt)
                file(REMOVE gcc.cmake)
            else()
                list(APPEND args ${CMAKE_ARGV${i}})
            endif()
        endforeach()
        execute_process(
            COMMAND
                cmake -G Ninja ${args} -C ${CMAKE_ARGV2} ${CMAKE_CURRENT_LIST_DIR}/..
            INPUT_FILE /dev/stdin
            OUTPUT_FILE /dev/stdout
            ERROR_FILE /dev/stderr
        )
        execute_process(
            COMMAND ninja kernel.elf
            INPUT_FILE /dev/stdin
            OUTPUT_FILE /dev/stdout
            ERROR_FILE /dev/stderr
        )
        execute_process(
            COMMAND ninja kernel_all_pp_wrapper
            INPUT_FILE /dev/stdin
            OUTPUT_FILE /dev/stdout
            ERROR_FILE /dev/stderr
        )
        return()
    endif()
endmacro()
