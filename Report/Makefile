OUT = report

all: $(OUT).pdf

%.pdf: %.tex $(wildcard *.tex *.bib */*.eps)
	latexmk -pdf $<

clean:
	rm -f *~ *.bbl *.blg *.dvi *.log *.aux *.ent *.fls *.fdb_latexmk $(OUT).pdf

read: 
	open main.pdf