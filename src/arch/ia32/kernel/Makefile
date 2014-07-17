#
# Copyright 2014, General Dynamics C4 Systems
#
# This software may be distributed and modified according to the terms of
# the GNU General Public License version 2. Note that NO WARRANTY is provided.
# See "LICENSE_GPLv2.txt" for details.
#
# @TAG(GD_GPL)
#

DIRECTORIES += src/arch/$(ARCH)/kernel

ARCH_C_SOURCES += kernel/thread.c \
                  kernel/vspace.c \
                  kernel/apic.c \
                  kernel/boot_sys.c \
                  kernel/boot.c \
                  kernel/cmdline.c \
                  kernel/elf.c

ifdef DEBUG
ARCH_C_SOURCES += kernel/lock.c
endif
