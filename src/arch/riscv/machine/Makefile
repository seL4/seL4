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

DIRECTORIES += src/arch/riscv/machine

ARCH_C_SOURCES += machine/registerset.c \
    machine/hardware.c \
	machine/io.c

ifdef DEBUG
    ARCH_C_SOURCES += machine/capdl.c
endif
