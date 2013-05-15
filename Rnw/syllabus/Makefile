
# put all PDF targets here, separated by spaces
PDFS= syllabus.pdf

all: $(PDFS) 

clean:
	rm -f *.tex *.bbl *.blg *.aux *.out *.log *.spl 

%.pdf: %.Rnw
	R -e "library(knitr); knit2pdf('$*.Rnw')"

