# put all PDF targets here, separated by spaces
RNW = $(wildcard *.Rnw)
PDFS= syllabus.pdf

all: $(PDFS) 

clean:
	rm -f *.tex *.bbl *.blg *.aux *.out *.log *.spl 

%.pdf: %.Rnw $(RNW)
	R -e "knitr:::knit('$*.Rnw')"
	xelatex $*.tex
	bibtex  $*

