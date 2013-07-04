RMD = $(wildcard *.rmd)

MD = $(RMD:%.rmd=%.md)
PDF = $(RMD:%.rmd=%.pdf)
TEX = $(RMD:%.rmd=%.tex)
HTML = $(RMD:%.rmd=%.html)

.PHONY: clean pdf html

pdf: $(PDF)
tex: $(TEX)
html: $(HTML)
md: $(MD)
	
all: pdf html

%.md: %.rmd
	R -e "knitr::knit('$<','$@')"

%.pdf: %.md
	pandoc -t beamer -V theme:Malmoe --highlight-style=haddock $< -o $@

%.tex: %.md
	pandoc -s -t beamer -V theme:Malmoe $< -o $@ 

%.html: %.md
	pandoc $< -o $@

clean:
	rm -f $(PDF) $(HTML) $(TEX) $(MD)
