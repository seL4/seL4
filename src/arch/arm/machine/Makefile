#
# Copyright 2014, General Dynamics C4 Systems
#
# This software may be distributed and modified according to the terms of
# the GNU General Public License version 2. Note that NO WARRANTY is provided.
# See "LICENSE_GPLv2.txt" for details.
#
# @TAG(GD_GPL)
#

DIRECTORIES += src/arch/arm/machine

ARCH_C_SOURCES += machine/hardware.c \
                  machine/registerset.c \
                  machine/cache.c \
                  machine/errata.c

ifeq ($(CPU), cortex-a9)
    ARCH_C_SOURCES += machine/gic_pl390.c
    ARCH_C_SOURCES += machine/l2c_310.c

ifeq ($(PLAT), exynos4)
    # if exynos4412 has MPCORE peripherals, then
    # samsung has NOT documented their location
else
    ARCH_C_SOURCES += machine/priv_timer.c
endif

endif

ifeq ($(CPU), cortex-a15)
    # A15 uses the gic_pl400 which is an extention of pl390
    ARCH_C_SOURCES += machine/gic_pl390.c
endif

ifdef DEBUG
    ARCH_C_SOURCES += machine/debug.c machine/capdl.c
endif
