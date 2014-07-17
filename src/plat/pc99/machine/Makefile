#
# Copyright 2014, General Dynamics C4 Systems
#
# This software may be distributed and modified according to the terms of
# the GNU General Public License version 2. Note that NO WARRANTY is provided.
# See "LICENSE_GPLv2.txt" for details.
#
# @TAG(GD_GPL)
#

DIRECTORIES += src/plat/$(PLAT)/machine

PLAT_C_SOURCES += machine/acpi.c \
                  machine/hardware.c \
                  machine/pci.c \
                  machine/pic.c \
                  machine/pit.c \
                  machine/io.c
ifdef DEBUG
PLAT_C_SOURCES += machine/debug_helpers.c
endif

PLAT_C_SOURCES += machine/intel-vtd.c 
