#
# Copyright 2014, General Dynamics C4 Systems
#
# This software may be distributed and modified according to the terms of
# the GNU General Public License version 2. Note that NO WARRANTY is provided.
# See "LICENSE_GPLv2.txt" for details.
#
# @TAG(GD_GPL)
#

DIRECTORIES += src/arch/$(ARCH)/object

ifeq ($(__X86_32__),y)
ARCH_C_SOURCES += object/objecttype_32.c
endif
ifeq ($(__X86_64__),y)
ARCH_C_SOURCES += object/objecttype_64.c
endif

ARCH_C_SOURCES += object/interrupt.c \
                  object/ioport.c \
                  object/objecttype.c \
                  object/tcb.c

ARCH_C_SOURCES += object/iospace.c 
