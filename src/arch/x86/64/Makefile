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

DIRECTORIES += src/arch/$(ARCH)/64

include ${SOURCE_ROOT}/src/arch/$(ARCH)/64/object/Makefile
include ${SOURCE_ROOT}/src/arch/$(ARCH)/64/kernel/Makefile
include ${SOURCE_ROOT}/src/arch/$(ARCH)/64/model/Makefile
include ${SOURCE_ROOT}/src/arch/$(ARCH)/64/machine/Makefile
include ${SOURCE_ROOT}/src/arch/$(ARCH)/64/smp/Makefile

ARCH_C_SOURCES += 64/c_traps.c
ARCH_ASM_SOURCES += 64/machine_asm.S \
                    64/traps.S \
                    64/head.S
