#
# Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
#
# SPDX-License-Identifier: BSD-2-Clause
#

cmake_minimum_required(VERSION 3.16.0)

project(libsel4 C)

set(configure_string "")

config_choice(
    LibSel4FunctionAttributes
    LIB_SEL4_FUNCTION_ATTRIBUTE
    "Function attributes \
    default->Verification friendly default configuration. syscalls will be inlined, \
        but generated functions will not. \
    inline->When set to true will mark generated functions as 'inline', allowing \
        them to be inlined by the callee user code. This may be undesirable \
        for verification, so setting to 'n' will forcibly prevent the function \
        from being inlined. \
    public->When set to true will make all user facing functions available as \
        public symbols, which can be convenient for some language bindings."
    "inline;LibSel4FunctionAttributeInline;LIB_SEL4_INLINE_INVOCATIONS"
    "default;LibSel4FunctionAttributeDefault;LIB_SEL4_DEFAULT_FUNCTION_ATTRIBUTES"
    "public;LibSel4FunctionAttributePublic;LIB_SEL4_PUBLIC_SYMBOLS"
)

config_option(
    LibSel4StubsUseIPCBufferOnly
    LIB_SEL4_STUBS_USE_IPC_BUFFER_ONLY
    "use only IPC buffer for syscalls. When generating syscall wrappers, only use the \
    IPC buffer for marshalling and unmarshalling arguments. Without this option set, \
    arguments will be passed in registers where possible for better performance."
    DEFAULT OFF
)

config_string(
    LibSel4PrintInvocationErrors LIB_SEL4_PRINT_INVOCATION_ERRORS
    "Generated stubs on error will print the message contained in the IPC buffer"
    DEFAULT 1
    DEPENDS "KernelInvocationReportErrorIPC" DEFAULT_DISABLED 0
    UNQUOTE
)

if(LibSel4StubsUseIPCBufferOnly)
    set(buffer "--buffer")
endif()

RequireFile(SYSCALL_STUB_GEN_PATH syscall_stub_gen.py PATHS tools)

add_config_library(sel4 "${configure_string}")

# Currently we use autoconf.h, so generate one of those
generate_autoconf(sel4_autoconf "kernel;sel4")

gen_invocation_header(
    OUTPUT include/sel4/invocation.h
    XML include/interfaces/object-api.xml
    LIBSEL4
)

gen_invocation_header(
    OUTPUT sel4_arch_include/${KernelSel4Arch}/sel4/sel4_arch/invocation.h
    XML "${CMAKE_CURRENT_SOURCE_DIR}/sel4_arch_include/${KernelSel4Arch}/interfaces/object-api-sel4-arch.xml"
    LIBSEL4 SEL4ARCH
)

gen_invocation_header(
    OUTPUT arch_include/${KernelArch}/sel4/arch/invocation.h
    XML "${CMAKE_CURRENT_SOURCE_DIR}/arch_include/${KernelArch}/interfaces/object-api-arch.xml"
    LIBSEL4 ARCH
)

set(source_header_dirs
    "${CMAKE_CURRENT_SOURCE_DIR}/include"
    "${CMAKE_CURRENT_SOURCE_DIR}/arch_include/${KernelArch}"
    "${CMAKE_CURRENT_SOURCE_DIR}/sel4_arch_include/${KernelSel4Arch}"
    "${CMAKE_CURRENT_SOURCE_DIR}/sel4_plat_include/${KernelPlatform}"
    "${CMAKE_CURRENT_SOURCE_DIR}/mode_include/${KernelWordSize}"
)
include_directories("${source_header_dirs}")

# Add the include directory of autoconf.h to the cflags for the bitfield generation
include_directories("$<TARGET_PROPERTY:sel4_autoconf,INTERFACE_INCLUDE_DIRECTORIES>")

function(genbf target_prefix pbf_location bf_location header_output)
    get_generated_files(gen_list sel4_autoconf_Gen)
    cppfile(
        "${pbf_location}" ${target_prefix}_pbf "${bf_location}"
        EXTRA_FLAGS -P
        EXTRA_DEPS sel4_autoconf_Gen ${gen_list}
    )
    GenHBFTarget(
        "libsel4"
        ${target_prefix}_h
        "${header_output}"
        "${pbf_location}"
        ${target_prefix}_pbf
        ""
        ""
        "${bf_location}"
    )
endfunction(genbf)

genbf("libsel4_shared_types_gen" "${CMAKE_CURRENT_BINARY_DIR}/include/sel4/shared_types.pbf"
      "${CMAKE_CURRENT_SOURCE_DIR}/mode_include/${KernelWordSize}/sel4/shared_types.bf"
      "${CMAKE_CURRENT_BINARY_DIR}/include/sel4/shared_types_gen.h"
)

genbf(
    "libsel4_sel4_arch_types_gen"
    "${CMAKE_CURRENT_BINARY_DIR}/sel4_arch_include/${KernelSel4Arch}/sel4/sel4_arch/types.pbf"
    "${CMAKE_CURRENT_SOURCE_DIR}/sel4_arch_include/${KernelSel4Arch}/sel4/sel4_arch/types.bf"
    "${CMAKE_CURRENT_BINARY_DIR}/sel4_arch_include/${KernelSel4Arch}/sel4/sel4_arch/types_gen.h"
)

if(KernelIsMCS)
    set(mcs --mcs)
endif()
if(KernelX86_64VTX64BitGuests)
    set(64bitguests --x86-vtx-64-bit-guests)
endif()
add_custom_command(
    OUTPUT include/sel4/syscall.h
    COMMAND rm -f include/sel4/syscall.h
    COMMAND "${XMLLINT_PATH}" --noout --schema "${CMAKE_CURRENT_SOURCE_DIR}/include/api/syscall.xsd"
            "${CMAKE_CURRENT_SOURCE_DIR}/include/api/syscall.xml"
    COMMAND
        ${PYTHON3} "${SYSCALL_ID_GEN_PATH}" --xml
        "${CMAKE_CURRENT_SOURCE_DIR}/include/api/syscall.xml" --libsel4_header
        include/sel4/syscall.h ${mcs}
    DEPENDS "${SYSCALL_ID_GEN_PATH}" "${CMAKE_CURRENT_SOURCE_DIR}/include/api/syscall.xsd"
            "${CMAKE_CURRENT_SOURCE_DIR}/include/api/syscall.xml"
    COMMENT "Generate syscall.h"
    VERBATIM
)

set(interface_xmls
    "${CMAKE_CURRENT_SOURCE_DIR}/sel4_arch_include/${KernelSel4Arch}/interfaces/object-api-sel4-arch.xml"
    "${CMAKE_CURRENT_SOURCE_DIR}/arch_include/${KernelArch}/interfaces/object-api-arch.xml"
    "${CMAKE_CURRENT_SOURCE_DIR}/include/interfaces/object-api.xml"
)

add_custom_command(
    OUTPUT include/interfaces/sel4_client.h
    COMMAND rm -f include/interfaces/sel4_client.h
    COMMAND "${PYTHON3}" "${SYSCALL_STUB_GEN_PATH}" ${buffer} ${64bitguests} ${mcs} -a
            "${KernelSel4Arch}" -o include/interfaces/sel4_client.h ${interface_xmls}
    DEPENDS "${SYSCALL_STUB_GEN_PATH}" ${interface_xmls}
    COMMENT "Generate sel4_client.h"
    VERBATIM
)

add_custom_target(
    sel4_generated
    DEPENDS include/interfaces/sel4_client.h
            include/sel4/syscall.h
            include/sel4/invocation.h
            arch_include/${KernelArch}/sel4/arch/invocation.h
            include/sel4/shared_types_gen.h
            sel4_arch_include/${KernelSel4Arch}/sel4/sel4_arch/invocation.h
            sel4_arch_include/${KernelSel4Arch}/sel4/sel4_arch/types_gen.h
)

add_library(sel4 src/sel4_bootinfo.c)
target_link_libraries(sel4 PRIVATE kernel_Config sel4_Config sel4_autoconf)
target_include_directories(
    sel4
    PUBLIC ${source_header_dirs} "${CMAKE_CURRENT_BINARY_DIR}/include"
           "${CMAKE_CURRENT_BINARY_DIR}/arch_include/${KernelArch}"
           "${CMAKE_CURRENT_BINARY_DIR}/sel4_arch_include/${KernelSel4Arch}"
)
add_dependencies(sel4 sel4_generated)
