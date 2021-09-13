#
# Copyright 2020, Data61, CSIRO (ABN 41 687 119 230)
#
# SPDX-License-Identifier: GPL-2.0-only
#

cmake_minimum_required(VERSION 3.7.2)

register_driver(compatibility_strings "arm,mmu-500" PREFIX src/drivers/smmu CFILES "smmuv2.c")
