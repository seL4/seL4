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

cmake_minimum_required(VERSION 3.7.2)

add_sources(
    DEP "KernelArchArmV6"
    PREFIX src/arch/arm/armv/armv6
    CFILES
        benchmark.c
        cache.c
        user_access.c
    ASMFILES
        machine_asm.S
)
