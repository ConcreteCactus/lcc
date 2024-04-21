.PHONY: docs

docs: docs.pdf

docs.pdf: high-level-architecture.diag.pdf \
		  syntax-checker.diag.pdf \
		  semantic-analyzer.diag.pdf \
		  code-generator.diag.pdf \
		  testing-methodology.diag.pdf \
		  elte-logo.pdf \
		  docs.tex
	latexmk -pdf docs.tex
	latexmk -c

%.diag.pdf: %.diag.plantuml
	plantuml -tpdf -SdefaultFontName="CMU Serif" $<

elte-logo.pdf: elte-logo.svg
	inkscape $< --export-type=pdf

clean:
	latexmk -c
	rm *.diag.tex *.diag.pdf *.dvi
