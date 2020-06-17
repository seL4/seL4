#
# Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
#
# SPDX-License-Identifier: GPL-2.0-only
#

cmake_minimum_required(VERSION 3.7.2)

add_sources(
    DEP "KernelArchArmV7a"
    PREFIX src/arch/arm/armv/armv7-a
    CFILES cache.c user_access.c tlb.c
    ASMFILES machine_asm.S
)
