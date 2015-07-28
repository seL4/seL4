#
# Copyright 2014, General Dynamics C4 Systems
#
# This software may be distributed and modified according to the terms of
# the GNU General Public License version 2. Note that NO WARRANTY is provided.
# See "LICENSE_GPLv2.txt" for details.
#
# @TAG(GD_GPL)
#

include ${SOURCE_ROOT}/src/arch/$(ARCH)/api/Makefile
include ${SOURCE_ROOT}/src/arch/$(ARCH)/object/Makefile
include ${SOURCE_ROOT}/src/arch/$(ARCH)/kernel/Makefile
include ${SOURCE_ROOT}/src/arch/$(ARCH)/model/Makefile
include ${SOURCE_ROOT}/src/arch/$(ARCH)/machine/Makefile
include ${SOURCE_ROOT}/src/arch/$(ARCH)/fastpath/Makefile

ARCH_C_SOURCES += c_traps.c

ARCH_ASM_SOURCES += machine_asm.S \
                    traps.S \
                    halt.S \
                    idle.S \
                    head.S
