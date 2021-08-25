#
# Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
#
# SPDX-License-Identifier: GPL-2.0-only
#

set(KERNEL_PATH "${CMAKE_CURRENT_LIST_DIR}" CACHE STRING "")
set(KERNEL_HELPERS_PATH "${CMAKE_CURRENT_LIST_DIR}/tools/helpers.cmake" CACHE STRING "")
set(KERNEL_CONFIG_PATH "${CMAKE_CURRENT_LIST_DIR}/configs/seL4Config.cmake" CACHE STRING "")
mark_as_advanced(KERNEL_PATH KERNEL_HELPERS_PATH KERNEL_CONFIG_PATH)

macro(sel4_import_kernel)
    add_subdirectory(${KERNEL_PATH} ${CMAKE_BINARY_DIR}/kernel)
endmacro()

macro(sel4_import_libsel4)
    add_subdirectory("${KERNEL_PATH}/libsel4" ${CMAKE_BINARY_DIR}/libsel4)
endmacro()

macro(sel4_configure_platform_settings)
    include(${KERNEL_CONFIG_PATH})
endmacro()

include(FindPackageHandleStandardArgs)
FIND_PACKAGE_HANDLE_STANDARD_ARGS(
    seL4
    DEFAULT_MSG
    KERNEL_PATH
    KERNEL_HELPERS_PATH
    KERNEL_CONFIG_PATH
)
