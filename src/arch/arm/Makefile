#
# Copyright 2014, General Dynamics C4 Systems
#
# This software may be distributed and modified according to the terms of
# the GNU General Public License version 2. Note that NO WARRANTY is provided.
# See "LICENSE_GPLv2.txt" for details.
#
# @TAG(GD_GPL)
#

INCLUDE_DIRS += ${SOURCE_ROOT}/include/arch/arm/armv/${ARMV}

include ${SOURCE_ROOT}/src/arch/arm/api/Makefile
include ${SOURCE_ROOT}/src/arch/arm/object/Makefile
include ${SOURCE_ROOT}/src/arch/arm/kernel/Makefile
include ${SOURCE_ROOT}/src/arch/arm/model/Makefile
include ${SOURCE_ROOT}/src/arch/arm/machine/Makefile
include ${SOURCE_ROOT}/src/arch/arm/armv/$(ARMV)/Makefile

ifeq ($(__ARM_32__),y)
include ${SOURCE_ROOT}/src/arch/$(ARCH)/32/Makefile
endif
