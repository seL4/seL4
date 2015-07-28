#
# Copyright 2014, General Dynamics C4 Systems
#
# This software may be distributed and modified according to the terms of
# the GNU General Public License version 2. Note that NO WARRANTY is provided.
# See "LICENSE_GPLv2.txt" for details.
#
# @TAG(GD_GPL)
#


BOOT_MODULES = Kernel/CSpace Kernel/Thread Kernel/FaultHandler \
    Kernel/VSpace Kernel/Init Model/PSpace Object/TCB Object/CNode \
    Object/ObjectType

BOOT_FILES=$(BOOT_MODULES:%=src/SEL4/%.lhs-boot)

all: build pdf
	
build: $(BOOT_FILES)
	cabal sandbox init
	cabal install --dependencies-only
	cabal configure
	cabal build

pdf:
	cd doc && make

%.lhs-boot: %.lhs
	perl mkhsboot.pl -l < $< > $@

clean:
	rm -f $(BOOT_FILES)
	cabal clean
