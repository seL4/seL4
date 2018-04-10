#
# Copyright 2018, Data61
# Commonwealth Scientific and Industrial Research Organisation (CSIRO)
# ABN 41 687 119 230.
#
# This software may be distributed and modified according to the terms of
# the GNU General Public License version 2. Note that NO WARRANTY is provided.
# See "LICENSE_GPLv2.txt" for details.
#
# @TAG(DATA61_GPL)
#

#
#
# Copyright 2016, 2017 Hesham Almatary, Data61/CSIRO <hesham.almatary@data61.csiro.au>
# Copyright 2015, 2016 Hesham Almatary <heshamelmatary@gmail.com>
#

DIRECTORIES += src/arch/riscv/

include ${SOURCE_ROOT}/src/arch/riscv/object/Makefile
include ${SOURCE_ROOT}/src/arch/riscv/api/Makefile
include ${SOURCE_ROOT}/src/arch/riscv/kernel/Makefile
include ${SOURCE_ROOT}/src/arch/riscv/model/Makefile
include ${SOURCE_ROOT}/src/arch/riscv/machine/Makefile

ARCH_ASM_SOURCES += head.S traps.S
ARCH_C_SOURCES += c_traps.c idle.c
