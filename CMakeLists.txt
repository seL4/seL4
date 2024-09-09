#
# Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
# Copyright 2024, Capabilities Limited
# CHERI support contributed by Capabilities Limited was developed by Hesham Almatary
#
# SPDX-License-Identifier: GPL-2.0-only
#

cmake_minimum_required(VERSION 3.8.2)
include(CheckCCompilerFlag)

include(${CMAKE_CURRENT_LIST_DIR}/configs/seL4Config.cmake)
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
set(PYTHON3 "python3" CACHE INTERNAL "")
RequireTool(CPP_GEN_PATH cpp_gen.sh)
RequireTool(CIRCULAR_INCLUDES circular_includes.py)
RequireTool(CONFIG_GEN_PATH config_gen.py)
RequireTool(HARDWARE_GEN_PATH hardware_gen.py)
RequireTool(INVOCATION_ID_GEN_PATH invocation_header_gen.py)
RequireTool(SYSCALL_ID_GEN_PATH syscall_header_gen.py)
RequireTool(XMLLINT_PATH xmllint.sh)

# Process the configuration scripts
include(config.cmake)

if(CheriPureCapKernel)
    # Custom bitfield generator for purecap seL4 kernel that
    # breaks the proofs.
    RequireTool(BF_GEN_PATH bitfield_gen_cheri.py)
else()
    RequireTool(BF_GEN_PATH bitfield_gen.py)
endif()

# Define default global flag information so that users can compile with the same
# basic architecture flags as the kernel

string(APPEND common_flags " -D__KERNEL_${KernelWordSize}__")

# Setup kernel specific flags used for both the compilation and linking step
macro(KernelCommonFlags)
    foreach(common_flag IN ITEMS ${ARGV})
        add_compile_options(${common_flag})
        string(APPEND CMAKE_EXE_LINKER_FLAGS " ${common_flag} ")
    endforeach()
endmacro(KernelCommonFlags)

if(KernelArchX86)
    if(${KernelX86MicroArch} STREQUAL "generic")
        string(APPEND common_flags " -mtune=generic")
    else()
        string(APPEND common_flags " -march=${KernelX86MicroArch}")
    endif()

    if(KernelSel4ArchX86_64)
        string(APPEND common_exe_flags " -Wl,-m,elf_x86_64")
    elseif(KernelSel4ArchIA32)
        string(APPEND common_exe_flags " -Wl,-m,elf_i386")
    else()
        message(FATAL_ERROR "unknown x86 KernelSel4Arch '${KernelSel4Arch}'")
    endif()

    string(APPEND c_common_flags " -m${KernelWordSize}")

    if(LLVM_TOOLCHAIN)
        if(KernelSel4ArchIA32)
            string(APPEND asm_common_flags " -m32")
        endif()
    else() # default is GCC toolchain
        string(APPEND asm_common_flags " -Wa,--${KernelWordSize}")
    endif()

elseif(KernelArchARM)
    if(HaveCheri)
        # Only Morello has CHERI support for Arm
        set(arm_march "morello${KernelArmMachFeatureModifiers}")

        if(CheriPureCapKernel)
            KernelCommonFlags(-mabi=purecap)
        elseif(CheriHybridKernel)
            KernelCommonFlags(-mabi=aapcs)
        else()
          message(FATAL_ERROR "seL4 can only be hybrid or purecap when CHERI is enabled")
        endif()
    else()
        set(arm_march "${KernelArmArmV}${KernelArmMachFeatureModifiers}")
    endif()

    string(APPEND c_common_flags " -march=${arm_march}")
    string(APPEND asm_common_flags " -march=${arm_march}")

    if(KernelSel4ArchAarch64)
        # nothing special
    elseif(KernelSel4ArchAarch32)
        # Explicitly request ARM instead of THUMB for compilation.
        string(APPEND c_common_flags " -marm")
    else()
        message(FATAL_ERROR "unknown ARM KernelSel4Arch '${KernelSel4Arch}'")
    endif()

elseif(KernelArchRiscV)
    # The "RISC-V Instruction Set Manual, Volume I: RISC-V User-Level ISA",
    # chapter "ISA Extension Naming Conventions" describes the march string. Its
    # form is roughly "rv(32|64)((im?a?f?d?)|g)q?c?b?...", where 'g' equals
    # 'mafd_Zicsr_Zifencei'. Underscores are allowed as extension separator and
    # even required between for all Z-extensions.

    if(KernelSel4ArchRiscV32)
        set(_riscv_march "rv32i") # Currently we don't support "rv32e"

        if(HaveCheri)
            # User is always purecap if CHERI is enabled
            set(_riscv_mabi "il32pc64")

            if(CheriHybridKernel)
                set(_riscv_mabi_kernel "ilp32")
            else()
                set(_riscv_mabi_kernel "il32pc64")
            endif()
        else()
            set(_riscv_mabi "ilp32")
            set(_riscv_mabi_kernel "ilp32")
        endif()

    elseif(KernelSel4ArchRiscV64)
        set(_riscv_march "rv64i")

        if(HaveCheri)
            # User is always purecap if CHERI is enabled
            set(_riscv_mabi "l64pc128")

            if(CheriHybridKernel)
                set(_riscv_mabi "lp64")
            else()
                set(_riscv_mabi_kernel "l64pc128")
            endif()
        else()
            set(_riscv_mabi "lp64")
            set(_riscv_mabi_kernel "lp64")
        endif()

    else()
        message(FATAL_ERROR "unknown RISC-V KernelSel4Arch '${KernelSel4Arch}'")
    endif()

    # Currently there are no KernelRiscvExtM or KernelRiscvExtA, thus the
    # M-extension and A-extension are always enabled.
    string(APPEND _riscv_march "ma")

    # FPU support. The Q-extension implies the D-extension, which implies the
    # F-extension, which implies the "Zicsr"-extension. Currently there is no
    # KernelRiscvExtQ yet.
    if(KernelRiscvExtD)
        # Since the D-extension implies F-extension, specifying "d" would be
        # sufficient. But it's common practice to say "fd"
        string(APPEND _riscv_march "fd")
        # Pass floating-point values up to 64 bits in F registers
        string(APPEND _riscv_mabi "d")
    elseif(KernelRiscvExtF)
        string(APPEND _riscv_march "f")
        # Pass floating-point values up to 32 bits in F registers
        string(APPEND _riscv_mabi "f")
    endif()

    # Currently there is no KernelRiscvExtC and thus the C-extension is always
    # enabled
    string(APPEND _riscv_march "c")

    # Determine if GNU toolchain is used and if yes, whether GCC version >= 11.3 (implies binutils version >= 2.38)
    if(
        CMAKE_ASM_COMPILER_ID STREQUAL "GNU"
        AND CMAKE_C_COMPILER_VERSION VERSION_GREATER_EQUAL "11.3"
    )
        # Manually enable Zicsr and Zifencei extensions
        # This became necessary due to a change in the default ISA version in GNU binutils 2.38 which is the
        # default binutils version shipped with GCC 11.3
        string(APPEND _riscv_march "_zicsr_zifencei")
    endif()

    if(HaveCheri)
        # The "x" is for non-standard CHERI extension. This is going to change to "z" once
        # CHERI is standardised in RISC-V.
        string(APPEND _riscv_march "_xcheri")
    else()
        # CHERI mode lets non-kernel subsystems to use different ABIs than itself. Here, we
        # just add an -mabi for compatibility with non-CHERI builds.
        string(APPEND common_flags " -mabi=${_riscv_mabi} ")
    endif()

    string(APPEND common_flags " -march=${_riscv_march} ")
    KernelCommonFlags(-mabi=${_riscv_mabi_kernel})

    # Clang/lld don't support linker relaxation yet
    if(CMAKE_C_COMPILER_ID STREQUAL "Clang")
        string(APPEND common_flags " -mno-relax")
    endif()

else()
    message(FATAL_ERROR "unknown KernelArch '${KernelArch}'")
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

set(
    BASE_EXE_LINKER_FLAGS "${common_flags} ${common_exe_flags} "
    CACHE INTERNAL "Default flags for linker an elf binary application"
)

# Setup kernel specific flags. Initialize all flags them from the same base
# flags that the users will use. The kernel is a stand-alone application with a
# custom implementation of the standard libraries.
include(${KERNEL_FLAGS_PATH})

add_compile_options(
    -std=c99
    #-----------------------------------
    # Configure warnings
    #-----------------------------------
    -Wall
    -Werror
    -Wstrict-prototypes
    -Wmissing-prototypes
    -Wnested-externs
    -Wmissing-declarations
    -Wundef
    -Wpointer-arith
    -Wno-nonnull
    #-----------------------------------
    # Configure compiler settings.
    #-----------------------------------
    -nostdinc # Do not use any system include paths, only use those given
              # explicitly by the "-I <path>" parameters.
    -ffreestanding # implies "-fno-builtin". Execution will not start at main().
                   # No assumptions about the meaning of function names from the
                   # standard library are made, except for memcpy(), memmove(),
                   # memset() and memcmp(). __builtin_trap() will call abort().
    -fno-stack-protector
    -fno-asynchronous-unwind-tables
    # GCC < 10 and clang < 11 put uninitialized global variables into a 'COMMON'
    # section unless '-fno-common' is specified. The linker will put anything
    # from 'COMMON' as the end of the '.bss' if nothing else is specified in the
    # linker script. Besides making the variable placement look odd, this also
    # tends to waste a page because we puts large aligned block at the end.
    # Eventually, GCC 10 and clang 11 made '-fno-common' the default, see
    # - https://gcc.gnu.org/gcc-10/changes.html
    # - https://releases.llvm.org/11.0.0/tools/clang/docs/ReleaseNotes.html
    -fno-common
)

# Linker parameters. There are two kinds actually:
#   - flags processed by GCC when invoking it as linker wrapper
#   - flags passed by GCC to the actual linker invocation ("-Wl,<flag>").
string(
    APPEND CMAKE_EXE_LINKER_FLAGS
    # KernelCommonFlags adds "-nostdlib", it's the GCC linker step counterpart
    # for "-ffreestanding" and makes GCC not use the standard system startup
    # files or libraries. This also excludes GCC's helper library libgcc. Any
    # libraries that are to be linked must be specified explicitly. Tests have
    # shown that this parameter doesn't prevent GCC from adding paths from a
    # "-L <path>" argument to the linker invocation for the standard libs, and
    # there seems no option that prevents this apart from providing an entirely
    # different specs file via -specs=<file>. Note that "-Wl,-nostdlib" is not
    # used here, because it is not needed. It makes the linker use library
    # directories specified on the command line only and ignore any SEARCH_DIR
    # set in a linker script. We provide our own linker scripts, and these
    # don't set SEARCH_DIR.
    " -static" # Implies "-no-pie" (and overrides "-pie"). The ld 2.37 docs say
               # "-no-pie" is a linker option, but passing "-Wl,-no-pie" fails.
    " -Wl,--build-id=none" # Ensure reproducible builds
    " -Wl,-n" # Disable page alignment of sections
)

KernelCommonFlags(
    ${KernelOptimisation}
    # The following options are gcc options, it is unclear if ld options are
    # generated automatically when gcc wraps the linking step and invokes ld.
    -nostdlib -fno-pic -fno-pie
)

# Disable cloned functions. This is needed for binary verification at -O2.
if(NOT KernelOptimisationCloneFunctions AND (CMAKE_C_COMPILER_ID STREQUAL "GNU"))
    KernelCommonFlags(-fno-partial-inlining -fno-ipa-cp -fno-ipa-sra)
endif()

if(KernelFWholeProgram)
    # KernelFWholeProgram is still an experimental feature and disabled by
    # default. Clarify if the linker step via GCC actually cares about this
    # parameter. There are also the options -flto and -fuse-linker-plugin that
    # might be a more modern approach.
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

if(KernelArchX86)
    if(KernelSel4ArchX86_64)
        KernelCommonFlags(-mcmodel=kernel)
    elseif(KernelSel4ArchIA32)
        # nothing special
    else()
        message(FATAL_ERROR "unknown x86 KernelSel4Arch '${KernelSel4Arch}'")
    endif()
    add_compile_options(-mno-mmx -mno-sse -mno-sse2 -mno-3dnow)

elseif(KernelArchARM)
    if(KernelSel4ArchAarch64)
        KernelCommonFlags(-mgeneral-regs-only)
        if(
            ((CMAKE_C_COMPILER_ID STREQUAL "GNU")
             AND (CMAKE_C_COMPILER_VERSION VERSION_GREATER_EQUAL "10.0.0")
             )
            OR
                ((CMAKE_C_COMPILER_ID STREQUAL "Clang")
                 AND (CMAKE_C_COMPILER_VERSION VERSION_GREATER_EQUAL "12.0.0")
                 )
        )
            add_compile_options(-mno-outline-atomics)
        endif()
    elseif(KernelSel4ArchAarch32)
        KernelCommonFlags(-mfloat-abi=soft)
    else()
        message(FATAL_ERROR "unknown ARM KernelSel4Arch '${KernelSel4Arch}'")
    endif()

elseif(KernelArchRiscV)
    # Group "small" data objects together in a small-data section so they can
    # be referenced using gp-relative addressing.  The exact value of
    # small-data-limit is not crucial but it should be lowered if .small
    # exceeds 4KiB.
    #
    KernelCommonFlags(-mcmodel=medany -msmall-data-limit=1024)

else()
    message(FATAL_ERROR "unknown KernelArch '${KernelArch}'")
endif()

# Sort the C sources to ensure a stable layout of the final C file
list(SORT c_sources)
# Add the domain schedule now that its sorted
list(APPEND c_sources "${KernelDomainSchedule}")

# Add static header includes
include_directories(
    "include"
    "include/${KernelWordSize}"
    "include/arch/${KernelArch}"
    "include/arch/${KernelArch}/arch/${KernelWordSize}"
    "include/plat/${KernelPlatform}"
    "include/plat/${KernelPlatform}/plat/${KernelWordSize}"
)

if(KernelArchARM)
    include_directories(
        "include/arch/arm/armv/${KernelArmArmV}"
        "include/arch/arm/armv/${KernelArmArmV}/${KernelWordSize}"
    )
endif()

if(KernelArmMach STREQUAL "exynos")
    include_directories("include/plat/exynos_common/")
endif()

# Add CHERI helper headers
if(HaveCheri)
    include_directories("include/cheri")
endif()

# Add libsel4 include directories. These are explicitly added instead of calling
# target_link_libraries(${target} sel4) because we don't want to inherit any
# other build options from libsel4.
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
# The kernel expects to be able to include an 'autoconf.h' file at the moment.
# So lets generate one for it to use
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
    XML
        ${CMAKE_CURRENT_SOURCE_DIR}/libsel4/arch_include/${KernelArch}/interfaces/object-api-arch.xml
    ARCH
)
list(APPEND xml_headers "${header_dest}")
list(APPEND gen_files_list "${header_dest}")

set(header_dest "gen_headers/arch/api/sel4_invocation.h")
gen_invocation_header(
    OUTPUT "${header_dest}"
    XML
        "${CMAKE_CURRENT_SOURCE_DIR}/libsel4/sel4_arch_include/${KernelSel4Arch}/interfaces/object-api-sel4-arch.xml"
    SEL4ARCH
)
list(APPEND xml_headers "${header_dest}")
list(APPEND gen_files_list "${header_dest}")

set(header_dest "gen_headers/api/invocation.h")
gen_invocation_header(
    OUTPUT "${header_dest}"
    XML "${CMAKE_CURRENT_SOURCE_DIR}/libsel4/include/interfaces/object-api.xml"
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
    kernel_all_pp_prune.c kernel_all_pp_prune_wrapper kernel_all.c
    EXTRA_FLAGS -CC "-I${CMAKE_CURRENT_BINARY_DIR}/generated_prune"
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
        "${pbf_name}" "${pbf_target}" "${bf_file}"
        EXTRA_FLAGS -P
        EXTRA_DEPS kernel_config_headers ${gen_files_list}
    )
    GenHBFTarget(
        ""
        ${bf_gen_target}
        "generated/${bf_gen_dir}/${bf_target}"
        "${pbf_name}"
        "${pbf_target}"
        "kernel_all_pp_prune.c"
        "kernel_all_pp_prune_wrapper"
        "${bf_file}"
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
    kernel_all.i kernel_i_wrapper kernel_all.c
    EXTRA_DEPS kernel_all_c_wrapper kernel_headers ${gen_files_list}
    EXTRA_FLAGS
        -CC "${CPPExtraFlags}"
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
    "${linker_lds_path}" linker_ld_wrapper "${linker_source}"
    EXTRA_DEPS kernel_headers ${gen_files_list}
    EXTRA_FLAGS -CC -P "${CPPExtraFlags}"
)

add_custom_command(
    OUTPUT circular_includes_valid
    COMMAND ${PYTHON3} ${CIRCULAR_INCLUDES} --ignore kernel_all_copy.c < kernel_all.i
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

# The following commands setup the install target for copying generated files and
# compilation outputs to an install location: CMAKE_INSTALL_PREFIX.
# CMAKE_INSTALL_PREFIX can be set on the cmake command line.
#
# The current installation outputs are:
# - ${CMAKE_INSTALL_PREFIX}/bin/kernel.elf: Location of kernel.elf binary
# - ${CMAKE_INSTALL_PREFIX}/libsel4/include: The include root for libsel4
# - ${CMAKE_INSTALL_PREFIX}/libsel4/src: The c source files for the libsel4 library
#
# The install target is only created if this is the top level project.
# We don't currently support creating install targets if the kernel is
# imported in another project.
if("${CMAKE_SOURCE_DIR}" STREQUAL "${CMAKE_CURRENT_SOURCE_DIR}")

    # Import libsel4 to get access to generation targets
    add_subdirectory(libsel4)
    # Add a default target that builds kernel.elf and generates all libsel4 headers
    add_custom_target(single-project ALL DEPENDS sel4_generated kernel.elf)
    # Disable the libsel4.a target as we don't intend to build the libsel4 sources
    set_target_properties(sel4 PROPERTIES EXCLUDE_FROM_ALL ON)
    # Install kernel.elf to bin/kernel.elf
    install(TARGETS kernel.elf RUNTIME DESTINATION bin)
    # Install all libsel4 headers to libsel4/include
    install(
        DIRECTORY
            "${CMAKE_CURRENT_SOURCE_DIR}/libsel4/include/"
            "${CMAKE_CURRENT_SOURCE_DIR}/libsel4/arch_include/${KernelArch}/"
            "${CMAKE_CURRENT_SOURCE_DIR}/libsel4/sel4_arch_include/${KernelSel4Arch}/"
            "${CMAKE_CURRENT_SOURCE_DIR}/libsel4/sel4_plat_include/${KernelPlatform}/"
            "${CMAKE_CURRENT_SOURCE_DIR}/libsel4/mode_include/${KernelWordSize}/"
            "${CMAKE_CURRENT_BINARY_DIR}/libsel4/include/"
            "${CMAKE_CURRENT_BINARY_DIR}/libsel4/arch_include/${KernelArch}/"
            "${CMAKE_CURRENT_BINARY_DIR}/libsel4/sel4_arch_include/${KernelSel4Arch}/"
            # The following directories install the autoconf headers
            "${CMAKE_CURRENT_BINARY_DIR}/gen_config/"
            "${CMAKE_CURRENT_BINARY_DIR}/libsel4/gen_config/"
            "${CMAKE_CURRENT_BINARY_DIR}/libsel4/autoconf/"
        DESTINATION libsel4/include
        FILES_MATCHING
        PATTERN "*.h"
        PATTERN "*.pbf"
        PATTERN "api/syscall.xml"
        PATTERN "api/syscall.xsd"
        PATTERN "object-api*.xml"
        PATTERN "gen_config.json"
    )
    # Install libsel4 sources to libsel4/src
    install(DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}/libsel4/src/" DESTINATION libsel4/src)
    # Install additional support files
    if(DEFINED KernelDTBPath)
        install(FILES ${KernelDTBPath} DESTINATION support)
    endif()
    if(DEFINED platform_yaml)
        install(FILES ${platform_yaml} DESTINATION support)
    endif()
    if(DEFINED platform_json)
        install(FILES ${platform_json} DESTINATION support)
    endif()

endif()
