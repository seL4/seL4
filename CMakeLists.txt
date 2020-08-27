#
# Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
#
# SPDX-License-Identifier: GPL-2.0-only
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

# Define tools used by the kernel
set(PYTHON "python2" CACHE INTERNAL "")
set(PYTHON3 "python3" CACHE INTERNAL "")
RequireTool(CPP_GEN_PATH cpp_gen.sh)
RequireTool(CIRCULAR_INCLUDES circular_includes.py)
RequireTool(BF_GEN_PATH bitfield_gen.py)
RequireTool(HARDWARE_GEN_PATH hardware_gen.py)
RequireTool(INVOCATION_ID_GEN_PATH invocation_header_gen.py)
RequireTool(SYSCALL_ID_GEN_PATH syscall_header_gen.py)
RequireTool(XMLLINT_PATH xmllint.sh)

set(c_sources "")
set(asm_sources "")
set(bf_declarations "")
foreach(file IN LISTS config_c_sources)
    list(APPEND c_sources "${CMAKE_CURRENT_SOURCE_DIR}/${file}")
endforeach()
foreach(file IN LISTS config_asm_sources)
    list(APPEND asm_sources "${CMAKE_CURRENT_SOURCE_DIR}/${file}")
endforeach()
foreach(file IN LISTS config_bf_declarations)
    list(APPEND bf_declarations "${CMAKE_CURRENT_SOURCE_DIR}/${file}")
endforeach()
set(KernelDTSList "${config_KernelDTSList}")

# Process the configuration scripts
include(config.cmake)

# Define default global flag information so that users can compile with the same basic architecture
# flags as the kernel
if(KernelArchX86)
    if(${KernelX86MicroArch} STREQUAL generic)
        set(build_arch "-mtune=generic")
    else()
        set(build_arch "-march=${KernelX86MicroArch}")
    endif()
    if(Kernel64)
        if(NOT LLVM_TOOLCHAIN)
            string(APPEND asm_common_flags " -Wa,--64")
        endif()
        string(APPEND c_common_flags " -m64")
    else()
        if(NOT LLVM_TOOLCHAIN)
            string(APPEND asm_common_flags " -Wa,--32")
        else()
            string(APPEND asm_common_flags " -m32")
        endif()
        string(APPEND c_common_flags " -m32")
    endif()
endif()
if(KernelArchARM)
    set(arm_march "${KernelArmArmV}${KernelArmMachFeatureModifiers}")
    string(APPEND c_common_flags " -march=${arm_march}")
    string(APPEND asm_common_flags " -march=${arm_march}")
    # Explicitly request ARM instead of THUMB for compilation. This option is not
    # relevant on aarch64
    if(NOT KernelSel4ArchAarch64)
        string(APPEND c_common_flags " -marm")
    endif()
endif()
if(KernelArchRiscV)
    if(Kernel64)
        if(KernelHaveFPU)
            string(APPEND common_flags " -march=rv64imafdc")
            string(APPEND common_flags " -mabi=lp64d")
        else()
            string(APPEND common_flags " -march=rv64imac")
            string(APPEND common_flags " -mabi=lp64")
        endif()
    else()
        string(APPEND common_flags " -march=rv32imac")
        string(APPEND common_flags " -mabi=ilp32")
    endif()
endif()
string(APPEND common_flags " ${build_arch}")
if(Kernel64)
    string(APPEND common_flags " -D__KERNEL_64__")
else()
    string(APPEND common_flags " -D__KERNEL_32__")
endif()

set(
    BASE_ASM_FLAGS "${asm_common_flags} ${common_flags}"
    CACHE INTERNAL "Default ASM flags for compilation \
    (subset of flags used by the kernel build)"
)
set(
    BASE_C_FLAGS "${c_common_flags} ${common_flags}"
    CACHE INTERNAL "Default C flags for compilation \
    (subset of flags used by the kernel)"
)
set(
    BASE_CXX_FLAGS "${cxx_common_flags} ${c_common_flags} ${common_flags}"
    CACHE INTERNAL "Default CXX flags for compilation"
)
if(KernelArchX86)
    if(Kernel64)
        string(APPEND common_exe_flags " -Wl,-m -Wl,elf_x86_64")
    else()
        string(APPEND common_exe_flags " -Wl,-m -Wl,elf_i386")
    endif()
endif()
set(
    BASE_EXE_LINKER_FLAGS "${common_flags} ${common_exe_flags} "
    CACHE INTERNAL "Default flags for linker an elf binary application"
)
# Initializing the kernel build flags starting from the same base flags that the users will use
include(${KERNEL_FLAGS_PATH})

# Setup kernel specific flags
macro(KernelCommonFlags)
    foreach(common_flag IN ITEMS ${ARGV})
        add_compile_options(${common_flag})
        string(APPEND CMAKE_EXE_LINKER_FLAGS " ${common_flag} ")
    endforeach()
endmacro(KernelCommonFlags)
KernelCommonFlags(-nostdinc -nostdlib ${KernelOptimisation} -DHAVE_AUTOCONF)
if(KernelFWholeProgram)
    KernelCommonFlags(-fwhole-program)
endif()
if(KernelDebugBuild)
    KernelCommonFlags(-DDEBUG -g -ggdb)
    # Pretend to CMake that we're a release build with debug info. This is because
    # we do actually allow CMake to do the final link step, so we'd like it not to
    # strip our binary
    set(CMAKE_BUILD_TYPE "RelWithDebInfo")
else()
    set(CMAKE_BUILD_TYPE "Release")
endif()
if(KernelArchX86 AND Kernel64)
    KernelCommonFlags(-mcmodel=kernel)
endif()
if(KernelArchARM)
    if(KernelSel4ArchAarch64)
        KernelCommonFlags(-mgeneral-regs-only)
    else()
        KernelCommonFlags(-mfloat-abi=soft)
    endif()
endif()
if(KernelArchRiscV)
    KernelCommonFlags(-mcmodel=medany)
endif()
KernelCommonFlags(-fno-pic -fno-pie)
add_compile_options(
    -fno-stack-protector
    -fno-asynchronous-unwind-tables
    -std=c99
    -Wall
    -Werror
    -Wstrict-prototypes
    -Wmissing-prototypes
    -Wnested-externs
    -Wmissing-declarations
    -Wundef
    -Wpointer-arith
    -Wno-nonnull
    -ffreestanding
)

# Add all the common flags to the linker args
string(APPEND CMAKE_EXE_LINKER_FLAGS " -ffreestanding -Wl,--build-id=none -static -Wl,-n ")

if(KernelArchX86)
    add_compile_options(-mno-mmx -mno-sse -mno-sse2 -mno-3dnow)
endif()

# Sort the C sources to ensure a stable layout of the final C file
list(SORT c_sources)
# Add the domain schedule now that its sorted
list(APPEND c_sources "${KernelDomainSchedule}")

# Add static header includes
include_directories("include")
include_directories("include/${KernelWordSize}")
include_directories("include/arch/${KernelArch}")
include_directories("include/arch/${KernelArch}/arch/${KernelWordSize}/")
include_directories("include/plat/${KernelPlatform}/")
include_directories("include/plat/${KernelPlatform}/plat/${KernelWordSize}/")
if(KernelArchARM)
    include_directories("include/arch/arm/armv/${KernelArmArmV}/")
    include_directories("include/arch/arm/armv/${KernelArmArmV}/${KernelWordSize}")
endif()
if(KernelArmMach STREQUAL "exynos")
    include_directories("include/plat/exynos_common/")
endif()
if(KernelArchRiscV)
    include_directories("include/arch/${KernelArch}/arch/${KernelWordSize}")
    include_directories("include/plat/${KernelPlatform}/plat/${KernelWordSize}")
endif()

# Add libsel4 include directories.
# These are explicitly added instead of calling target_link_libraries(${target} sel4)
# because we don't want to inherit any other build options from libsel4.
include_directories(
    "libsel4/include"
    "libsel4/arch_include/${KernelArch}"
    "libsel4/sel4_arch_include/${KernelSel4Arch}"
    "libsel4/sel4_plat_include/${KernelPlatform}"
    "libsel4/mode_include/${KernelWordSize}"
)

#
# Config generation
#

include_directories($<TARGET_PROPERTY:kernel_Config,INTERFACE_INCLUDE_DIRECTORIES>)
# The kernel expects to be able to include an 'autoconf.h' file at the moment. So lets
# generate one for it to use
# TODO: use the kernel_Config directly
generate_autoconf(kernel_autoconf "kernel")
include_directories($<TARGET_PROPERTY:kernel_autoconf,INTERFACE_INCLUDE_DIRECTORIES>)

# Target for the config / autoconf headers. This is what all the other generated headers
# can depend upon
add_custom_target(
    kernel_config_headers
    DEPENDS
        kernel_autoconf_Gen
        kernel_autoconf
        kernel_Config
        kernel_Gen
)

# Target for all generated headers. We start with just all the config / autoconf headers
add_custom_target(kernel_headers DEPENDS kernel_config_headers)

# Build up a list of generated files. needed for dependencies in custom commands
get_generated_files(gen_files_list kernel_autoconf_Gen)
get_generated_files(gen_files2 kernel_Gen)
list(APPEND gen_files_list "${gen_files2}")

#
# C source generation
#

# Kernel compiles all C sources as a single C file, this provides
# rules for doing the concatenation

add_custom_command(
    OUTPUT kernel_all.c
    COMMAND
        "${CPP_GEN_PATH}" ${c_sources} > kernel_all.c
    DEPENDS "${CPP_GEN_PATH}" ${c_sources}
    COMMENT "Concatenating C files"
    VERBATIM
)

add_custom_target(kernel_all_c_wrapper DEPENDS kernel_all.c)

#
# Header Generation
#

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
    XML
        "${CMAKE_CURRENT_SOURCE_DIR}/libsel4/sel4_arch_include/${KernelSel4Arch}/interfaces/sel4arch.xml"
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

set(syscall_xml_base "${CMAKE_CURRENT_SOURCE_DIR}/libsel4/include/api")
set(syscall_dest "gen_headers/arch/api/syscall.h")
if(KernelIsMCS)
    set(mcs --mcs)
endif()
add_custom_command(
    OUTPUT ${syscall_dest}
    COMMAND
        "${XMLLINT_PATH}"
        --noout
        --schema "${syscall_xml_base}/syscall.xsd" "${syscall_xml_base}/syscall.xml"
    COMMAND
        ${CMAKE_COMMAND} -E remove -f "${syscall_dest}"
    COMMAND
        ${PYTHON3} "${SYSCALL_ID_GEN_PATH}"
        --xml "${syscall_xml_base}/syscall.xml"
        --kernel_header "${syscall_dest}" ${mcs}
    DEPENDS
        "${XMLLINT_PATH}"
        "${SYSCALL_ID_GEN_PATH}"
        "${syscall_xml_base}/syscall.xsd"
        "${syscall_xml_base}/syscall.xml"
    COMMENT "Generate syscall invocations"
    VERBATIM
)
list(APPEND xml_headers "${syscall_dest}")
list(APPEND gen_files_list "${syscall_dest}")
# Construct target for just the xml headers
add_custom_target(xml_headers_target DEPENDS ${xml_headers})
# Add the xml headers to all the kernel headers
add_dependencies(kernel_headers xml_headers_target)
include_directories("${CMAKE_CURRENT_BINARY_DIR}/gen_headers")

#
# Prune list generation
#

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
    string(
        REPLACE
            ":"
            ";"
            bf_dec
            ${bf_dec}
    )
    list(GET bf_dec 0 bf_file)
    list(GET bf_dec 1 bf_gen_dir)
    get_filename_component(bf_name "${bf_file}" NAME)
    string(
        REPLACE
            ".bf"
            "_gen.h"
            bf_target
            "${bf_name}"
    )
    list(
        APPEND
            dummy_headers "${CMAKE_CURRENT_BINARY_DIR}/generated_prune/${bf_gen_dir}/${bf_target}"
    )
endforeach()

add_custom_command(
    OUTPUT ${dummy_headers}
    COMMAND
        ${CMAKE_COMMAND} -E touch ${dummy_headers}
    COMMENT "Generate dummy headers for prune compilation"
)

add_custom_target(dummy_header_wrapper DEPENDS ${dummy_headers})

cppfile(
    kernel_all_pp_prune.c
    kernel_all_pp_prune_wrapper
    kernel_all.c
    EXTRA_FLAGS
    -CC
    "-I${CMAKE_CURRENT_BINARY_DIR}/generated_prune"
    EXTRA_DEPS
    kernel_all_c_wrapper
    dummy_header_wrapper
    xml_headers_target
    kernel_config_headers
    ${gen_files_list}
)

#
# Bitfield header generation
#

# Need to generate a bunch of unique targets, we'll do this with piano numbers
set(bf_gen_target "kernel_bf_gen_target_1")

foreach(bf_dec ${bf_declarations})
    string(
        REPLACE
            ":"
            ";"
            bf_dec
            ${bf_dec}
    )
    list(GET bf_dec 0 bf_file)
    list(GET bf_dec 1 bf_gen_dir)
    get_filename_component(bf_name "${bf_file}" NAME)
    string(
        REPLACE
            ".bf"
            "_gen.h"
            bf_target
            "${bf_name}"
    )
    string(
        REPLACE
            ".bf"
            "_defs.thy"
            defs_target
            "${bf_name}"
    )
    string(
        REPLACE
            ".bf"
            "_proofs.thy"
            proofs_target
            "${bf_name}"
    )
    set(pbf_name "generated/${bf_gen_dir}/${bf_name}.pbf")
    set(pbf_target "${bf_gen_target}_pbf")
    cppfile(
        "${pbf_name}"
        "${pbf_target}"
        "${bf_file}"
        EXTRA_FLAGS
        -P
        EXTRA_DEPS
        kernel_config_headers
        ${gen_files_list}
    )
    GenHBFTarget(
        ""
        ${bf_gen_target}
        "generated/${bf_gen_dir}/${bf_target}"
        "${pbf_name}"
        "${pbf_target}"
        "kernel_all_pp_prune.c"
        "kernel_all_pp_prune_wrapper"
    )
    GenDefsBFTarget(
        "${bf_gen_target}_def"
        "generated/${bf_gen_dir}/${defs_target}"
        "${pbf_name}"
        "${pbf_target}"
        "kernel_all_pp_prune.c"
        "kernel_all_pp_prune_wrapper"
    )
    GenProofsBFTarget(
        "${bf_gen_target}_proof"
        "generated/${bf_gen_dir}/${proofs_target}"
        "${pbf_name}"
        "${pbf_target}"
        "kernel_all_pp_prune.c"
        "kernel_all_pp_prune_wrapper"
    )
    list(
        APPEND
            theories_deps
            "${bf_gen_target}_def"
            "${CMAKE_CURRENT_BINARY_DIR}/generated/${bf_gen_dir}/${defs_target}"
            "${bf_gen_target}_proof"
            "${CMAKE_CURRENT_BINARY_DIR}/generated/${bf_gen_dir}/${proofs_target}"
    )
    add_dependencies(kernel_headers "${bf_gen_target}")
    list(APPEND gen_files_list "${CMAKE_CURRENT_BINARY_DIR}/generated/${bf_gen_dir}/${bf_target}")
    set(bf_gen_target "${bf_gen_target}1")
endforeach()
# At this point we have generated a bunch of headers into ${CMAKE_CURRENT_BINARY_DIR}/generated
# but we do not pass this to include_directories, as that will cause it to be an include directory
# for *all* targets in this file (including ones we defined earlier) and the prune generation
# *must not* see this files and generate dependencies on them as this will result in nonsense.
# As such we must manually add this as an include directory to future targets
set(CPPExtraFlags "-I${CMAKE_CURRENT_BINARY_DIR}/generated")

#
# Kernel compilation
#

cppfile(
    kernel_all.i
    kernel_i_wrapper
    kernel_all.c
    EXTRA_DEPS
    kernel_all_c_wrapper
    kernel_headers
    ${gen_files_list}
    EXTRA_FLAGS
    -CC
    "${CPPExtraFlags}"
    # The circular_includes script relies upon parsing out exactly 'kernel_all_copy.c' as
    # a special case so we must ask cppfile to use this input name
    EXACT_NAME kernel_all_copy.c
)

# Explain to cmake that our object file is actually a C input file
set_property(SOURCE kernel_all.i PROPERTY LANGUAGE C)

if(KernelArchARM)
    set(linker_source "src/arch/arm/common_arm.lds")
elseif(KernelArchRiscV)
    set(linker_source "src/arch/riscv/common_riscv.lds")
else()
    set(linker_source "src/plat/${KernelPlatform}/linker.lds")
endif()
set(linker_lds_path "${CMAKE_CURRENT_BINARY_DIR}/linker.lds_pp")

# Preprocess the linker script
cppfile(
    "${linker_lds_path}"
    linker_ld_wrapper
    "${linker_source}"
    EXTRA_DEPS
    kernel_headers
    ${gen_files_list}
    EXTRA_FLAGS
    -CC
    -P
    "${CPPExtraFlags}"
)

add_custom_command(
    OUTPUT circular_includes_valid
    COMMAND ${CIRCULAR_INCLUDES} --ignore kernel_all_copy.c < kernel_all.i
    COMMAND touch circular_includes_valid
    DEPENDS kernel_i_wrapper kernel_all.i
)

add_custom_target(circular_includes DEPENDS circular_includes_valid)

add_custom_command(
    OUTPUT kernel_all_pp.c
    COMMAND
        ${CMAKE_COMMAND} -E copy kernel_all.i kernel_all_pp.c
    DEPENDS kernel_i_wrapper kernel_all.i
)
add_custom_target(kernel_all_pp_wrapper DEPENDS kernel_all_pp.c)

add_custom_target(kernel_theories DEPENDS ${theories_deps})

# Declare final kernel output
add_executable(kernel.elf EXCLUDE_FROM_ALL ${asm_sources} kernel_all.c)
target_include_directories(kernel.elf PRIVATE ${config_dir})
target_include_directories(kernel.elf PRIVATE include)
target_include_directories(kernel.elf PRIVATE "${CMAKE_CURRENT_BINARY_DIR}/generated")
target_link_libraries(kernel.elf PRIVATE kernel_Config kernel_autoconf)
set_property(TARGET kernel.elf APPEND_STRING PROPERTY LINK_FLAGS " -Wl,-T ${linker_lds_path} ")
set_target_properties(kernel.elf PROPERTIES LINK_DEPENDS "${linker_lds_path}")
add_dependencies(kernel.elf circular_includes)
