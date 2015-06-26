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

ARCH_C_SOURCES += kernel/vspace.c

ifeq ($(__X86_32__),y)
ARCH_C_SOURCES += kernel/vspace_32.c \
                  kernel/vspace_32paging.c \
                  kernel/vspace_pae.c \
                  kernel/thread_32.c
endif

ifeq ($(__X86_64__),y)
ARCH_C_SOURCES += kernel/vspace_64.c
endif

ARCH_C_SOURCES += kernel/apic.c \
                  kernel/boot_sys.c \
                  kernel/boot.c \
                  kernel/cmdline.c \
                  kernel/elf.c

ifdef DEBUG
ARCH_C_SOURCES += kernel/lock.c
endif

ifdef RELEASE_PRINTF
ARCH_C_SOURCES += kernel/lock.c
endif

ARCH_C_SOURCES += kernel/vspace.c
