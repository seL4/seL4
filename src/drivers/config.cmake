#
# Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
#
# SPDX-License-Identifier: GPL-2.0-only
#

cmake_minimum_required(VERSION 3.7.2)

macro(register_driver compatibility_strings match_strings)
    foreach(match_string IN ITEMS ${match_strings})
        list(FIND ${compatibility_strings} ${match_string} res)
        if(NOT (res EQUAL -1))
            add_sources(${ARGN})
            break()
        endif()
    endforeach()
endmacro()

include(src/drivers/serial/config.cmake)
include(src/drivers/smmu/config.cmake)
include(src/drivers/timer/config.cmake)