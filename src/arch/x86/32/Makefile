#
# Copyright 2014, General Dynamics C4 Systems
#
# This software may be distributed and modified according to the terms of
# the GNU General Public License version 2. Note that NO WARRANTY is provided.
# See "LICENSE_GPLv2.txt" for details.
#
# @TAG(GD_GPL)
#

DIRECTORIES += src/arch/$(ARCH)/32

include ${SOURCE_ROOT}/src/arch/$(ARCH)/32/object/Makefile
include ${SOURCE_ROOT}/src/arch/$(ARCH)/32/kernel/Makefile
include ${SOURCE_ROOT}/src/arch/$(ARCH)/32/model/Makefile
include ${SOURCE_ROOT}/src/arch/$(ARCH)/32/machine/Makefile
include ${SOURCE_ROOT}/src/arch/$(ARCH)/32/smp/Makefile

ARCH_C_SOURCES += 32/c_traps.c
ARCH_ASM_SOURCES += 32/machine_asm.S \
                    32/traps.S \
                    32/head.S
