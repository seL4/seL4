#
# Copyright 2014, General Dynamics C4 Systems
#
# This software may be distributed and modified according to the terms of
# the GNU General Public License version 2. Note that NO WARRANTY is provided.
# See "LICENSE_GPLv2.txt" for details.
#
# @TAG(GD_GPL)
#

DIRECTORIES += src/arch/$(ARCH)/model

ifeq ($(__X86_32__),y)
ARCH_C_SOURCES += model/statedata_32.c
endif
ifeq ($(__X86_64__),y)
ARCH_C_SOURCES += model/statedata_64.c
endif

ARCH_C_SOURCES += model/statedata.c
