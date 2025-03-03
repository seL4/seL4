#
# Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
#
# SPDX-License-Identifier: GPL-2.0-only
#

cmake_minimum_required(VERSION 3.16.0)

add_sources(
    DEP "KernelArchArmV8a"
    PREFIX src/arch/arm/armv/armv8-a/${KernelWordSize}
    CFILES cache.c user_access.c
    ASMFILES machine_asm.S
)
