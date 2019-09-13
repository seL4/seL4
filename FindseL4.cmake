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
