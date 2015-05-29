#
# Copyright 2014, General Dynamics C4 Systems
#
# This software may be distributed and modified according to the terms of
# the GNU General Public License version 2. Note that NO WARRANTY is provided.
# See "LICENSE_GPLv2.txt" for details.
#
# @TAG(GD_GPL)
#

DIRECTORIES += src/arch/$(ARCH)/machine

ARCH_C_SOURCES += machine/hardware.c \
                  machine/registerset.c \
                  machine/fpu.c

ifdef DEBUG
    ARCH_C_SOURCES += machine/capdl.c
endif
