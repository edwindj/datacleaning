MD = $(wildcard *.md)

PDF = $(MD:%.md=%.pdf)
TEX = $(MD:%.md=%.tex)
HTML = $(MD:%.md=%.html)

.PHONY: clean pdf html

pdf: $(PDF)

tex: $(TEX)

html: $(HTML)
	
all: pdf

%.pdf: %.md
	pandoc -t beamer -V theme:Malmoe $< -o $@

%.tex: %.md
	pandoc -t beamer -V theme:Malmoe $< -o $@

%.html: %.md
	pandoc $< -o $@

clean:
	rm -f $(PDF) $(HTML) $(TEX)