#
# Copyright 2014, General Dynamics C4 Systems
#
# This software may be distributed and modified according to the terms of
# the GNU General Public License version 2. Note that NO WARRANTY is provided.
# See "LICENSE_GPLv2.txt" for details.
#
# @TAG(GD_GPL)
#

dftPath ?= ./dft/

dftExtraTeXInputs=

Target = refman.pdf

all: ${Target}

include ${dftPath}/rules/main.mk

LHSFILES=$(wildcard ../src/SEL4/*.lhs) \
	$(wildcard ../src/SEL4/Machine/*.lhs) \
	$(wildcard ../src/SEL4/Model/*.lhs) \
	$(wildcard ../src/SEL4/Object/*.lhs)\
	$(wildcard ../src/SEL4/Kernel/*.lhs)\
	$(wildcard ../src/SEL4/API/*.lhs)\
	$(wildcard ../src/SEL4/Machine/Arch/*.lhs)

TEXFILES=$(wildcard *.tex)

FIGURE_SOURCES=$(wildcard figures/*.mp)
FIGURES=$(patsubst %.mp,%.pdf,$(FIGURE_SOURCES))

SOURCES=$(LHSFILES) $(TEXFILES) $(FIGURES) version.aux functions.aux

%.mps: %.mp
	echo $< $@
	mpost $< && mv -f $(notdir $(@:.mps=.1)) $@

figures/%.pdf: figures/%.mps
	(cd figures && mptopdf $*.mps && mv $*-mps.pdf $*.pdf)

version.aux: always
	echo "\softwareversion{`hg parent --template '{date|shortdate} (Revision {node|short})\n'`}" > $@

.PHONY: always

functions.aux: $(LHSFILES)
	perl mkfunctions.pl

${Target}: ${SOURCES}

clean:
	rm -f *.aux *.toc *.bbl *.blg *.dvi *.log *.out \
		figures/*.mps figures/*.pdf figures/*.log ${Target}

# arch-tag: b2d385a6-aa7d-4404-86e2-722e2adc58cd
