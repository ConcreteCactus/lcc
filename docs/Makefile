.PHONY: docs

docs: docs.pdf

docs.pdf: high-level-architecture.diag.pdf \
		  syntax-checker.diag.pdf \
		  semantic-analyzer.diag.pdf \
		  code-generator.diag.pdf \
		  docs.tex
	latexmk -pdf docs.tex
	latexmk -c

%.diag.pdf: %.diag.tex
	latexmk -pdf $<
	latexmk -c

%.diag.tex: %.diag.plantuml
	plantuml -tlatex $<

clean:
	latexmk -c
	rm *.diag.tex *.pdf *.dvi
