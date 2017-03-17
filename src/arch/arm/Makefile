#
# Copyright 2014, General Dynamics C4 Systems
#
# This software may be distributed and modified according to the terms of
# the GNU General Public License version 2. Note that NO WARRANTY is provided.
# See "LICENSE_GPLv2.txt" for details.
#
# @TAG(GD_GPL)
#

include ${SOURCE_ROOT}/src/arch/arm/api/Makefile
include ${SOURCE_ROOT}/src/arch/arm/object/Makefile
include ${SOURCE_ROOT}/src/arch/arm/kernel/Makefile
include ${SOURCE_ROOT}/src/arch/arm/machine/Makefile
include ${SOURCE_ROOT}/src/arch/arm/benchmark/Makefile
include ${SOURCE_ROOT}/src/arch/arm/smp/Makefile
include ${SOURCE_ROOT}/src/arch/arm/armv/$(ARMV)/Makefile

include ${SOURCE_ROOT}/src/arch/$(ARCH)/${TYPE_SUFFIX}/Makefile

ARCH_C_SOURCES += c_traps.c
