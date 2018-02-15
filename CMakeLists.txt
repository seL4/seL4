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

cmake_minimum_required(VERSION 3.8.2)
include(CheckCCompilerFlag)
project(seL4 C ASM)

# First find our helpers
find_file(KERNEL_HELPERS_PATH helpers.cmake PATHS tools CMAKE_FIND_ROOT_PATH_BOTH)
mark_as_advanced(FORCE KERNEL_HELPERS_PATH)
include(${KERNEL_HELPERS_PATH})

function(RequireTool config file)
    RequireFile("${config}" "${file}" PATHS tools)
endfunction(RequireTool)

RequireTool(KERNEL_FLAGS_PATH flags.cmake)

if(CCACHEFOUND)
    set(ccache "ccache")
endif()

include(tools/internal.cmake)

# Process the configuration scripts
include(config.cmake)

# Define tools used by the kernel
set(PYTHON "python" CACHE INTERNAL "")
RequireTool(CPP_GEN_PATH cpp_gen.sh)
RequireTool(CIRCULAR_INCLUDES circular_includes.py)
RequireTool(BF_GEN_PATH bitfield_gen.py)
RequireTool(INVOCATION_ID_GEN_PATH invocation_header_gen.py)
RequireTool(SYSCALL_ID_GEN_PATH syscall_header_gen.py)
RequireTool(XMLLINT_PATH xmllint.sh)

# Define default global flag information so that users can compile with the same basic architecture
# flags as the kernel
if(KernelArchX86)
    if(${KernelX86MicroArch} STREQUAL generic)
        set(build_arch "-mtune=generic")
    else()
        set(build_arch "-march=${KernelX86MicroArch}")
    endif()
    if(Kernel64)
        set(asm_common_flags "${asm_common_flags} -Wa,--64")
        set(c_common_flags "${c_common_flags} -m64")
    else()
        set(asm_common_flags "${asm_common_flags} -Wa,--32")
        set(c_common_flags "${c_common_flags} -m32")
    endif()
endif()
if(KernelArchARM)
    set(arm_march "${KernelArmArmV}${KernelArmMachFeatureModifiers}")
    set(c_common_flags "${c_common_flags} -march=${arm_march}")
    set(asm_common_flags "${asm_common_flags} -Wa,-march=${arm_march}")
endif()
set(common_flags "${common_flags} ${build_arch}")
if(Kernel64)
    set(common_flags "${common_flags} -D__KERNEL_64__")
else()
    set(common_flags "${common_flags} -D__KERNEL_32__")
endif()

set(BASE_ASM_FLAGS "${asm_common_flags} ${common_flags}" CACHE INTERNAL "Default ASM flags for compilation \
    (subset of flags used by the kernel build)"
)
set(BASE_C_FLAGS "${c_common_flags} ${common_flags}" CACHE INTERNAL "Default C flags for compilation \
    (subset of flags used by the kernel)"
)
set(BASE_CXX_FLAGS "${cxx_common_flags} ${c_common_flags} ${common_flags}" CACHE INTERNAL
    "Default CXX flags for compilation"
)
if(KernelArchX86)
    if(Kernel64)
        set(common_exe_flags "${common_exe_flags} -Wl,-m -Wl,elf_x86_64")
    else()
        set(common_exe_flags "${common_exe_flags} -Wl,-m -Wl,elf_i386")
    endif()
endif()
set(BASE_EXE_LINKER_FLAGS "${common_flags} ${common_exe_flags} " CACHE INTERNAL
    "Default flags for linker an elf binary application"
)
# Initializing the kernel build flags starting from the same base flags that the users will use
include(${KERNEL_FLAGS_PATH})

# Setup kernel specific flags
set(kernel_common_flags "${kernel_common_flags} -nostdinc -nostdlib ${KernelOptimisation} -DHAVE_AUTOCONF")
if(KernelFWholeProgram)
    set(kernel_common_flags "${kernel_common_flags} -fwhole-program")
endif()
if(KernelDebugBuild)
    set(kernel_common_flags "${kernel_common_flags} -DDEBUG -g -ggdb")
    # Pretend to CMake that we're a release build with debug info. This is because
    # we do actually allow CMake to do the final link step, so we'd like it not to
    # strip our binary
    set(CMAKE_BUILD_TYPE "RelWithDebInfo")
else()
    set(CMAKE_BUILD_TYPE "Release")
endif()
if(KernelArchX86 AND Kernel64)
    set(kernel_common_flags "${kernel_common_flags} -mcmodel=kernel")
endif()
if(kernelArchARM)
    set(kernel_common_flags "${kernel_common_flags} -mfloat-abi=soft")
endif()
set(kernel_common_flags "${kernel_common_flags} -fno-pic -fno-pie")
set(CMAKE_ASM_FLAGS "${CMAKE_ASM_FLAGS} ${kernel_common_flags}")
set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} ${kernel_common_flags} \
    -fno-stack-protector -fno-asynchronous-unwind-tables -std=c99 ${KernelOptimisation} \
    -Wall -Werror -Wstrict-prototypes -Wmissing-prototypes -Wnested-externs -Wmissing-declarations \
    -Wundef -Wpointer-arith -Wno-nonnull -ffreestanding"
)

set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} ${kernel_common_flags} \
    -ffreestanding -Wl,--build-id=none -static")

set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} -Wl,-n")

if(KernelArchX86)
    set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -mno-mmx -mno-sse -mno-sse2 -mno-3dnow")
endif()

# Sort the C sources to ensure a stable layout of the final C file
list(SORT c_sources)
# Add the domain schedule now that its sorted
list(APPEND c_sources "${KernelDomainSchedule}")

# We define some macros for keeping flags in sync
macro(add_flags)
    set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} ${ARGN}")
    set(CMAKE_ASM_FLAGS "${CMAKE_ASM_FLAGS} ${ARGN}")
endmacro(add_flags)
macro(AddHeader header)
    add_flags("-I${header}")
endmacro(AddHeader)

# Add static header includes
AddHeader("${CMAKE_CURRENT_SOURCE_DIR}/include")
AddHeader("${CMAKE_CURRENT_SOURCE_DIR}/include/${KernelWordSize}")
AddHeader("${CMAKE_CURRENT_SOURCE_DIR}/include/arch/${KernelArch}")
AddHeader("${CMAKE_CURRENT_SOURCE_DIR}/include/arch/${KernelArch}/arch/${KernelWordSize}/")
AddHeader("${CMAKE_CURRENT_SOURCE_DIR}/include/plat/${KernelPlatform}/")
AddHeader("${CMAKE_CURRENT_SOURCE_DIR}/include/plat/${KernelPlatform}/plat/${KernelWordSize}/")
if(KernelArchARM)
    AddHeader("${CMAKE_CURRENT_SOURCE_DIR}/include/arch/arm/armv/${KernelArmArmV}/")
    AddHeader("${CMAKE_CURRENT_SOURCE_DIR}/include/arch/arm/armv/${KernelArmArmV}/${KernelWordSize}")
endif()
if(KernelArmMach STREQUAL "exynos")
    AddHeader("${CMAKE_CURRENT_SOURCE_DIR}/include/plat/exynos_common/")
endif()

###################
# Config generation
###################

include_directories($<TARGET_PROPERTY:kernel_Config,INTERFACE_INCLUDE_DIRECTORIES>)
# The kernel expects to be able to include an 'autoconf.h' file at the moment. So lets
# generate one for it to use
# TODO: use the kernel_Config directly
generate_autoconf(kernel_autoconf "kernel")
# When doing custom compilations we need to add the config header directories to our
# include path, since we cannot get them automatically through link libraries. For
# this purpose we have an additional variable for arguments that may have generator
# expressions
set(custom_command_c_flags "${custom_command_c_flags} -I$<JOIN:$<TARGET_PROPERTY:kernel_autoconf,INTERFACE_INCLUDE_DIRECTORIES>, -I>")
include_directories($<TARGET_PROPERTY:kernel_autoconf,INTERFACE_INCLUDE_DIRECTORIES>)

# Target for the config / autoconf headers. This is what all the other generated headers
# can depend upon
add_custom_target(kernel_config_headers
    DEPENDS kernel_autoconf_Gen kernel_autoconf kernel_Config kernel_Gen
)

# Target for all generated headers. We start with just all the config / autoconf headers
add_custom_target(kernel_headers DEPENDS kernel_config_headers
)

# Build up a list of generated files. needed for dependencies in custom commands
get_generated_files(gen_files_list kernel_autoconf_Gen)
get_generated_files(gen_files2 kernel_Gen)
list(APPEND gen_files_list "${gen_files2}")

#####################
# C source generation
#####################

# Kernel compiles all C sources as a single C file, this provides
# rules for doing the concatenation

add_custom_command(OUTPUT kernel_all.c
    COMMAND "${CPP_GEN_PATH}" ${c_sources} > kernel_all.c
    DEPENDS "${CPP_GEN_PATH}" ${c_sources}
    COMMENT "Concatenating C files"
    VERBATIM
)

add_custom_target(kernel_all_c_wrapper
    DEPENDS kernel_all.c
)

###################
# Header Generation
###################

# Rules for generating invocation and syscall headers
# Aside from generating file rules for dependencies this section will also produce a target
# that can be depended upon (along with the desired files themselves) to control parallelism

set(xml_headers "")
set(header_dest "gen_headers/arch/api/invocation.h")
gen_invocation_header(
    OUTPUT ${header_dest}
    XML ${CMAKE_CURRENT_SOURCE_DIR}/libsel4/arch_include/${KernelArch}/interfaces/sel4arch.xml
    ARCH
)
list(APPEND xml_headers "${header_dest}")
list(APPEND gen_files_list "${header_dest}")

set(header_dest "gen_headers/arch/api/sel4_invocation.h")
gen_invocation_header(
    OUTPUT "${header_dest}"
    XML "${CMAKE_CURRENT_SOURCE_DIR}/libsel4/sel4_arch_include/${KernelSel4Arch}/interfaces/sel4arch.xml"
    SEL4ARCH
)
list(APPEND xml_headers "${header_dest}")
list(APPEND gen_files_list "${header_dest}")

set(header_dest "gen_headers/api/invocation.h")
gen_invocation_header(
    OUTPUT "${header_dest}"
    XML "${CMAKE_CURRENT_SOURCE_DIR}/libsel4/include/interfaces/sel4.xml"
)
list(APPEND xml_headers "${header_dest}")
list(APPEND gen_files_list "${header_dest}")

set(syscall_xml_base "${CMAKE_CURRENT_SOURCE_DIR}/include/api")
set(syscall_dest "gen_headers/arch/api/syscall.h")
add_custom_command(OUTPUT ${syscall_dest}
    COMMAND "${XMLLINT_PATH}" --noout --schema "${syscall_xml_base}/syscall.xsd" "${syscall_xml_base}/syscall.xml"
    COMMAND ${CMAKE_COMMAND} -E remove -f "${syscall_dest}"
    COMMAND "${SYSCALL_ID_GEN_PATH}" --xml "${syscall_xml_base}/syscall.xml" --kernel_header "${syscall_dest}"
    DEPENDS "${XMLLINT_PATH}" "${SYSCALL_ID_GEN_PATH}" "${syscall_xml_base}/syscall.xsd" "${syscall_xml_base}/syscall.xml"
    COMMENT "Generate syscall invocations"
    VERBATIM
)
AddHeader("${CMAKE_CURRENT_BINARY_DIR}/gen_headers")
list(APPEND xml_headers "${syscall_dest}")
list(APPEND gen_files_list "${syscall_dest}")
# Construct target for just the xml headers
add_custom_target(xml_headers_target DEPENDS ${xml_headers})
# Add the xml headers to all the kernel headers
add_dependencies(kernel_headers xml_headers_target)

#######################
# Prune list generation
#######################

# When generating bitfield files we can pass multiple '--prune' parameters that are source
# files that get searched for determing which bitfield functions are used. This allows the
# bitfield generator to only generate functions that are used. Whilst irrelevant for
# normal compilation, not generating unused functions has significant time savings for the
# automated verification tools

# To generate a prune file we 'build' the kernel (similar to the kernel_all_pp.c rule
# below) but strictly WITHOUT the generated header directory where the bitfield generated
# headers are. This means our preprocessed file will contain all the code used by the
# normal compilation, just without the bitfield headers (which we generate dummy versions of).
# If we allowed the bitfield headers to be included then we would have a circular
# dependency. As a result this rule comes *before* the Bitfield header generation section

set(dummy_headers "")
foreach(bf_dec ${bf_declarations})
    string(REPLACE ":" ";" bf_dec ${bf_dec})
    list(GET bf_dec 0 bf_file)
    list(GET bf_dec 1 bf_gen_dir)
    get_filename_component(bf_name "${bf_file}" NAME)
    string(REPLACE ".bf" "_gen.h" bf_target "${bf_name}")
    list(APPEND dummy_headers "${CMAKE_CURRENT_BINARY_DIR}/generated_prune/${bf_gen_dir}/${bf_target}")
endforeach()

add_custom_command(OUTPUT ${dummy_headers}
    COMMAND ${CMAKE_COMMAND} -E touch ${dummy_headers}
    COMMENT "Generate dummy headers for prune compilation"
)

add_custom_target(dummy_header_wrapper
    DEPENDS ${dummy_headers}
)

CPPFile(kernel_all_pp_prune.c kernel_all_pp_prune_wrapper kernel_all.c
    EXTRA_FLAGS -CC "-I${CMAKE_CURRENT_BINARY_DIR}/generated_prune"
    EXTRA_DEPS kernel_all_c_wrapper dummy_header_wrapper xml_headers_target kernel_config_headers ${gen_files_list}
)

############################
# Bitfield header generation
############################

# Need to generate a bunch of unique targets, we'll do this with piano numbers
set(bf_gen_target "kernel_bf_gen_target_1")

foreach(bf_dec ${bf_declarations})
    string(REPLACE ":" ";" bf_dec ${bf_dec})
    list(GET bf_dec 0 bf_file)
    list(GET bf_dec 1 bf_gen_dir)
    get_filename_component(bf_name "${bf_file}" NAME)
    string(REPLACE ".bf" "_gen.h" bf_target "${bf_name}")
    string(REPLACE ".bf" "_defs.thy" defs_target "${bf_name}")
    string(REPLACE ".bf" "_proofs.thy" proofs_target "${bf_name}")
    set(pbf_name "generated/${bf_gen_dir}/${bf_name}.pbf")
    set(pbf_target "${bf_gen_target}_pbf")
    GenCPPCommand(
        "${pbf_name}"
        "${bf_file}"
        EXTRA_FLAGS "${custom_command_c_flags} -P"
        TARGET "${pbf_target}"
        EXTRA_DEPS "${gen_files_list};xml_headers_target;kernel_config_headers"
    )
    GenHBFTarget("" ${bf_gen_target} "generated/${bf_gen_dir}/${bf_target}" "${pbf_name}" "${pbf_target}"
        "kernel_all_pp_prune.c" "kernel_all_pp_prune_wrapper")
    GenDefsBFTarget("${bf_gen_target}_def" "generated/${bf_gen_dir}/${defs_target}" "${pbf_name}" "${pbf_target}"
        "kernel_all_pp_prune.c" "kernel_all_pp_prune_wrapper")
    GenProofsBFTarget("${bf_gen_target}_proof" "generated/${bf_gen_dir}/${proofs_target}" "${pbf_name}" "${pbf_target}"
        "kernel_all_pp_prune.c" "kernel_all_pp_prune_wrapper")
    list(APPEND theories_deps
        "${bf_gen_target}_def" "${CMAKE_CURRENT_BINARY_DIR}/generated/${bf_gen_dir}/${defs_target}"
        "${bf_gen_target}_proof" "${CMAKE_CURRENT_BINARY_DIR}/generated/${bf_gen_dir}/${proofs_target}"
    )
    add_dependencies(kernel_headers "${bf_gen_target}")
    list(APPEND gen_files_list "${CMAKE_CURRENT_BINARY_DIR}/generated/${bf_gen_dir}/${bf_target}")
    set(bf_gen_target "${bf_gen_target}1")
endforeach()

AddHeader("${CMAKE_CURRENT_BINARY_DIR}/generated")

####################
# Kernel compilation
####################

GenCPPCommand(
    kernel_all_pp.c
    kernel_all.c
    EXTRA_FLAGS "${custom_command_c_flags} -CC"
    TARGET kernel_all_pp_wrapper
    EXTRA_DEPS "kernel_all_c_wrapper;kernel_headers;${gen_files_list}"
)

set(linker_lds_path "${CMAKE_CURRENT_BINARY_DIR}/linker.lds_pp")
set(linker_source "src/plat/${KernelPlatform}/linker.lds")
GenCPPCommand(
    "${linker_lds_path}"
    "${linker_source}"
    EXTRA_FLAGS "${custom_command_c_flags} -P"
    TARGET linker_ld_wrapper
    EXTRA_DEPS "kernel_headers;${gen_files_list}"
)

# Need a custom build command as we need to not have our already pre-processed file get
# processed again, which means either using a .ii file or explicitly saying -x cpp-output.
# Unfortunately  and cmake doesn't understand .ii files directly so we do the latter approach
separate_arguments(args NATIVE_COMMAND "${custom_command_c_flags} ${CMAKE_C_FLAGS}")
add_custom_command(OUTPUT kernel_all.o
    # Take the opportunity to check for circular includes
    COMMAND ${CIRCULAR_INCLUDES} < kernel_all_pp.c
    COMMAND ${ccache} ${CMAKE_C_COMPILER} -ffreestanding -c -o kernel_all.o "${args}" -x cpp-output kernel_all_pp.c
    # Cannot make the kernel.elf executable properly depend upon the linker script so depend upon it
    # here
    DEPENDS kernel_all_pp.c kernel_all_pp_wrapper kernel_headers ${gen_files_list} linker_ld_wrapper "${linker_lds_path}"
    VERBATIM
    COMMENT "Compiling concatenated kernel sources"
    COMMAND_EXPAND_LISTS
)

add_custom_target(kernel_all_wrapper
    DEPENDS kernel_all.o
)

add_custom_target(kernel_theories
    DEPENDS ${theories_deps}
)

# Declare final kernel output
add_executable(kernel.elf EXCLUDE_FROM_ALL ${asm_sources} kernel_all.o)
target_include_directories(kernel.elf PRIVATE ${config_dir})
target_include_directories(kernel.elf PRIVATE include)
target_link_libraries(kernel.elf PRIVATE kernel_Config kernel_autoconf)
add_dependencies(kernel.elf kernel_all_wrapper)
set_property(TARGET kernel.elf APPEND_STRING PROPERTY LINK_FLAGS " -T ${linker_lds_path} ")
