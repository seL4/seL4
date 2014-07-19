#
# Copyright 2014, General Dynamics C4 Systems
#
# This software may be distributed and modified according to the terms of
# the GNU General Public License version 2. Note that NO WARRANTY is provided.
# See "LICENSE_GPLv2.txt" for details.
#
# @TAG(GD_GPL)
#

all: refman.pdf

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

SOURCES=$(LHSFILES) $(TEXFILES) $(FIGURES) version.txt functions.aux

figures/%.mps: figures/%.mp
	@echo $< $@
	@(cd figures; mpost --tex=latex $* && mv -f $*.1 $*.mps)

figures/%.pdf: figures/%.mps
	@(cd figures && mptopdf $*.mps && mv $*-mps.pdf $*.pdf)

version.txt: always
	@echo "`git log -n 1 --date=short --abbrev=20 --abbrev-commit --format='%h %ad' HEAD`" > $@

.PHONY: always

functions.aux: $(LHSFILES)
	perl mkfunctions.pl

refman.pdf: ${SOURCES}
	pdflatex refman
	bibtex refman
	pdflatex refman
	pdflatex refman

clean:
	rm -f *.aux *.toc *.bbl *.blg *.dvi *.log *.out \
		figures/*.mps figures/*.pdf figures/*.log ${Target}

