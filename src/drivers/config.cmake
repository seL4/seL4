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
include(src/drivers/timer/config.cmake)
