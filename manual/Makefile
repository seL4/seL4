#
# Copyright 2014, General Dynamics C4 Systems
#
# This software may be distributed and modified according to the terms of
# the GNU General Public License version 2. Note that NO WARRANTY is provided.
# See "LICENSE_GPLv2.txt" for details.
#
# @TAG(GD_GPL)
#

# $Id: Makefile,v 1.26 2009-04-21 05:40:29 gernot Exp $
#
#  To create a LaTeX diff against the CVS HEAD revision, use the target
#  "diff" (or "viewdiff").
#  To use a revision other than HEAD, speciff DIFF=x on the 
#  make command line to choose the revision x.
#

# This Makefile contains bash'isms.
SHELL=bash

.PHONY: all

BIBDIR     ?= /home/disy/lib/BibTeX:../../../bibtex
LaTeXEnv   = TEXINPUTS=".:/home/disy/lib/TeX:/home/disy/lib/ps:${TEXINPUTS}:"
BibTexEnv  = BIBINPUTS=".:${BIBDIR}:${BIBINPUTS}:"
BibFiles   = defs,extra,combined,theses,os-other

LaTeX      = ${LaTeXEnv} pdflatex -interaction=nonstopmode
BibTeX     = ${BibTexEnv} bibtex
Fig2Eps    = fig2dev -L eps
Dia2Eps    = dia -t eps -e
GnuPlot	   = gnuplot
Eps2Pdf	   = epstopdf --outfile
PdfView	   = xpdf
#PdfView    = open -a preview
#PdfView    = acroread 
Lpr 	   = lpr
mv	   = mv
awk	   = awk
R	   = R

# To add a second target, simply append the basename of the .tex file here
Targets    = manual


# CONFIGURATION OPTIONS
# =====================

# Extra figures that aren't supplied as dia, gnuplot or fig sources
# (eg figures already supplied in PDF, or supplied in EPS).
# List with PDF extensions:
ExtraFigs= #imgs/eps-fig.pdf

# Any other stuff that may be needed

# END CONFIGURATION OPTIONS
# =========================

Optional = $(addsuffix -diff, $(Targets))
All = $(Targets) $(Optional)
Diffopts=--type=BWUNDERLINE #-c .latexdiffconfig --append-safecmd="Comment"

Perf_Sources = $(wildcard imgs/*.perf)
Dia_Sources = $(wildcard imgs/*.dia)
Fig_Sources = $(wildcard imgs/*.fig)
Gnuplot_Sources = $(wildcard imgs/*.gnuplot)
R_Sources = $(wildcard imgs/*.r)
Figures = $(Perf_Sources:.perf=.pdf) $(Dia_Sources:.dia=.pdf) $(Fig_Sources:.fig=.pdf) $(Gnuplot_Sources:.gnuplot=.pdf)  $(R_Sources:.r=.pdf) $(ExtraFigs)

Pdf = $(addsuffix .pdf, $(Targets))
Bib = references.bib
Tex = $(addsuffix .tex, $(Targets))
Diff_Pdf = $(addsuffix .pdf, $(Optional))

.PHONY: FORCE

all: pdf
diff: diff_pdf
FORCE:
ps: $(Ps)
pdf: $(Figures) Makefile $(Pdf)
diff_pdf: $(Figures) Makefile $(Diff_Pdf)

# Verbosity.
ifeq (${V},1)
Q:=
else
Q:=@
endif

# Fetch information from the environment that needs to go into the document.
env.tex: FORCE
	@echo 'ENV ->' $@
	${Q}python tools/gen_env.py $@

manual.pdf: VERSION env.tex manual.tex parts/*.tex parts/api/*.tex

%.pdf: %.perf tools/bargraph.pl
	@echo $< '->' $@
	${Q}${BarGraph} -pdf $< > $@

%.pdf: %.eps
	@echo $< '->' $@
	${Q}${Eps2Pdf} $@ $<

%.pdf: %.ps
	@echo $< '->' $@
	${Q}${Eps2Pdf} $@ $<

%.eps: %.dia
	@echo $< '->' $@
	${Q}${Dia2Eps} $@ $<

%.eps: %.fig
	@echo $< '->' $@
	${Q}${Fig2Eps} $< $@

%.eps: %.gnuplot
	@echo $< '->' $@
	${Q}${GnuPlot} $<

%.eps: %.r
	@echo $< '->' $@
	${Q}${R} --vanilla < $<

view: pdf
	${Q}for i in $(Pdf); do \
		$(PdfView) $$i & \
	done

viewdiff: diff
	${Q}for i in $(Diff_Pdf); do \
		$(PdfView) $$i & \
	done

print: pdf
	${Q}for i in $(Pdf); do \
		$(Lpr) $$i \
	done

clean:
	rm -f *.aux *.toc *.bbl *.blg *.dvi *.log *.pstex* *.eps *.cb *.brf \
		*.out *.ps *-diff.tex *.mps .log *.pdf *.tgz *~ *.lof *.lot env.tex

tar:	clean
	( p=`pwd` && d=`basename "$$p"` && cd .. && \
	  tar cfz $$d.tgz $$d && \
	  mv $$d.tgz $$d )

help:
	@echo "Main targets: all diff view viewdiff print clean tar"
	@echo "'make diff' will show changes to head revision"
	@echo "'make DIFF=<rev> diff' will show changes to revision <rev>"

##############################################################################

DIFF ?= tip

%-diff.dvi: %-diff.tex

%-diff.tex: %.tex FORCE
	@echo "====> Retrieving revision $(DIFF) of $<"
	${Q}hg cat -r $(DIFF) $<  > $(@:-diff.tex=-$(DIFF)-diff.tex)
	@echo "====> Creating diff of revision $(DIFF) of $<"
	${Q}./tools/latexdiff $(Diffopts) $(@:-diff.tex=-$(DIFF)-diff.tex) $< > $@

.PHONY: FORCE
FORCE:

# don't delete %.aux intermediates
.SECONDARY:

##############################################################################

Rerun = '(There were undefined references|Rerun to get (cross-references|the bars) right)'
Rerun_Bib = 'No file.*\.bbl|Citation.*undefined'
Undefined = '((Reference|Citation).*undefined)|(Label.*multiply defined)'
Error = '^! '

# combine citation commands from all targets into tmp.aux, generate references.bib from this
references.bib: $(addsuffix .tex, $(Targets)) env.tex
	@echo "====> Parsing targets for references";
	${Q}for i in $(Targets); do \
		$(LaTeX) $$i.tex >>.log; \
		cat $$i.aux | grep -e "\(citation\|bibdata\|bibstyle\)" | sed 's/bibdata{references}/bibdata{$(BibFiles)}/g' >> all_refs.aux; \
	done
	@echo "====> Removing duplicate bib entries";
	${Q}cat all_refs.aux | uniq > tmp.aux;
	${Q}diff references.aux tmp.aux > references.diff 2> /dev/null; \
	if [ -s references.diff ] && [ -e references.bib ]; then \
		echo "====> Changed references:"; \
		cat references.diff | grep "citation"; \
		echo -n "These will cause changes to references.bib, do you want to rebuild this file? (yes/no): "; \
		read rebuild_refs; \
	fi; \
	if [ "$$rebuild_refs" == "yes" ] || [ \! -e references.bib ]; then \
		echo "====> Building references.bib"; \
		$(BibTexEnv) ./tools/bibexport.sh -t -o references.bib tmp.aux > /dev/null 2> /dev/null; \
		cp tmp.aux references.aux; \
	fi; 
	${Q}rm all_refs.aux tmp.aux references.diff

%.pdf: %.tex references.bib $(Figures) Makefile
	${Q}if ! test -e $*.bbl || test $(Bib) -nt $*.bbl; then rm -f $*.bbl; fi
	@echo "====> LaTeX first pass: $(<)"
	${Q}$(LaTeX) $< >.log || if egrep -q $(Error) $*.log ; then cat .log; rm $@; false ; fi
	${Q}if egrep -q $(Rerun_Bib) $*.log ; then echo "====> BibTex" && $(BibTeX) $* > /dev/null && echo "====> LaTeX BibTeX pass" && $(LaTeX) >.log $< ; fi
	${Q}if egrep -q $(Rerun) $*.log ; then echo "====> LaTeX rerun" && $(LaTeX) >.log $<; fi
	${Q}if egrep -q $(Rerun) $*.log ; then echo "====> LaTeX rerun" && $(LaTeX) >.log $<; fi
	${Q}if egrep -q $(Rerun) $*.log ; then echo "====> LaTeX rerun" && $(LaTeX) >.log $<; fi
	@echo "====> Undefined references and citations in $(<):"
	${Q}egrep -i $(Undefined) $*.log || echo "None."
	@echo "====> Dimensions:"
	${Q}grep "dimension:" $*.log || echo "None."

##############################################################################
# Generate a list of FIXMEs 
fixmes: 
	${Q}for i in $(Tex); do \
		echo "FIXMEs in $$i:"; \
		nl -b a $$i | grep "FIXME{" | nl -b a; \
		echo -n "Total FIXMES: " && grep "FIXME{" $$i | wc -l; \
		echo; \
	done
