# put all PDF targets here, separated by spaces
RNW = $(wildcard *.Rnw)
PDFS= syllabus.pdf

all: $(PDFS) 

clean:
	rm -f *.tex *.bbl *.blg *.aux *.out *.log *.spl *.toc
	rm -rf figure

%.pdf: %.Rnw $(RNW)
	R -e "knitr:::knit2pdf('$*.Rnw', compiler='xelatex')"

