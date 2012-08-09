### Makefile for Emacs refcards

## Copyright (C) 1993-1994, 2001-2012  Free Software Foundation, Inc.

## This file is part of GNU Emacs.
#
## GNU Emacs is free software: you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation, either version 3 of the License, or
## (at your option) any later version.

## GNU Emacs is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.

## You should have received a copy of the GNU General Public License
## along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.


## Top-level targets.

## PDF files that are included with Emacs.
all: refcards_pdf dired-refcards_pdf misc-refcards_pdf

REFCARDS_PDF = refcard.pdf cs-refcard.pdf de-refcard.pdf fr-refcard.pdf \
            pl-refcard.pdf pt-br-refcard.pdf ru-refcard.pdf sk-refcard.pdf
refcards_pdf: ${REFCARDS_PDF}
refcards_ps: ${REFCARDS_PDF:.pdf=.ps}

DIRED_REFCARDS_PDF = dired-ref.pdf cs-dired-ref.pdf fr-dired-ref.pdf \
		sk-dired-ref.pdf
dired-refcards_pdf: ${DIRED_REFCARDS_PDF}
dired-refcards_ps: ${DIRED_REFCARDS_PDF:.pdf=.ps}

MISC_REFCARDS_PDF = calccard.pdf gnus-booklet.pdf gnus-refcard.pdf orgcard.pdf
misc-refcards_pdf: ${MISC_REFCARDS_PDF}
misc-refcards_ps: ${MISC_REFCARDS_PDF:.pdf=.ps}


## The following files are not included with Emacs.
SURVIVAL_CARDS_PDF = survival.pdf cs-survival.pdf sk-survival.pdf
survival-cards_pdf: ${SURVIVAL_CARDS_PDF}
survival-cards_ps: ${SURVIVAL_CARDS_PDF:.pdf=.ps}

VIPER_CARDS_PDF = vipcard.pdf viperCard.pdf
viper-cards_pdf: ${VIPER_CARDS_PDF}
viper-cards_ps: ${VIPER_CARDS_PDF:.pdf=.ps}


## PDF files.

## The page layouts (a4/letter) are written directly in the .tex files.

cs-refcard.pdf cs-dired-ref.pdf cs-survival.pdf sk-refcard.pdf \
sk-dired-ref.pdf sk-survival.pdf: %.pdf:	%.tex
	if pdfcsplain --version > /dev/null 2> /dev/null; then \
	  pdfcsplain $<; \
	else \
	  csplain "\pdfoutput=1\input $<"; \
	fi

## Some versions of pdfmex seem to create dvi by default, hence output-format.
pl-refcard.pdf: %.pdf: %.tex
	if ! kpsewhich -format=fmt mex > /dev/null && \
	  ! pdfmex --version > /dev/null 2> /dev/null; then \
	  echo "No mex format found."; false; \
	fi
	pdftex -output-format=pdf $<

ru-refcard.pdf: %.pdf: %.tex
	pdflatex $<

#gnus-logo.pdf: %.pdf: %.eps
#	ps2pdf $<

gnus-refcard.pdf: %.pdf: %.tex gnus-logo.pdf
	pdflatex $<

gnus-booklet.pdf: gnus-refcard.tex gnus-logo.pdf
	pdflatex -jobname=gnus-booklet '\def\booklettrue{}\def\letterpapertrue{}\input{gnus-refcard}'

## Everything not explicitly listed above.
%.pdf: %.tex
	pdftex $<


## dvi files.

cs-refcard.dvi cs-dired-ref.dvi cs-survival.dvi sk-refcard.dvi \
sk-dired-ref.dvi sk-survival.dvi: %.dvi:	%.tex
	csplain $<

pl-refcard.dvi: %.dvi: %.tex
	if ! kpsewhich -format=fmt mex > /dev/null; then \
	  echo "No mex format found."; false; \
	fi
	tex $<

ru-refcard.dvi gnus-refcard.dvi: %.dvi: %.tex
	latex $<

gnus-booklet.dvi: gnus-refcard.tex
	latex '\def\booklettrue{}\def\letterpapertrue{}\input{gnus-refcard}'
	mv gnus-refcard.dvi $@

## Everything not explicitly listed above.
%.dvi: %.tex
	tex $<


## PostScript files.

## Note that some of the tex files (refcard, de-refcard, fr-refcard,
## pt-br-refcard) have settings for letter or a4 paper. Following are
## the default paper sizes (letter for English, A4 for translations).
## FIXME orgcard.ps does not fit on letter (see orgcard.tex).

## A4, landscape.
de-refcard.ps fr-refcard.ps pt-br-refcard.ps orgcard.ps: %.ps: %.dvi
	dvips -t a4 -t landscape $< -o $@

## A4, portrait.
cs-dired-ref.ps cs-refcard.ps cs-survival.ps fr-dired-ref.ps \
pl-refcard.ps ru-refcard.ps \
sk-dired-ref.ps sk-refcard.ps sk-survival.ps: %.ps: %.dvi
	dvips -t a4 $< -o $@

## letter, landscape.
calccard.ps refcard.ps: %.ps: %.dvi
	dvips -t letter -t landscape $< -o $@

## letter, portrait.
dired-ref.ps gnus-booklet.ps gnus-refcard.ps survival.ps vipcard.ps \
viperCard.ps: %.ps: %.dvi
	dvips -t letter $< -o $@


.PHONY: clean

clean:
	-rm -f *.dvi *.log *.aux

### Makefile ends here
