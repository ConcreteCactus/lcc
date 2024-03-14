.PHONY: docs

docs: docs.pdf high-level-architecture.diag.pdf

docs.pdf: high-level-architecture.diag.pdf docs.tex
	latexmk docs.tex
	latexmk -c

high-level-architecture.diag.pdf: high-level-architecture.diag.tex
	latexmk high-level-architecture.diag.tex
	latexmk -c

high-level-architecture.diag.tex: high-level-architecture.diag.plantuml
	java -jar ../plantuml.jar -tlatex high-level-architecture.diag.plantuml

clean:
	rm *.diag.tex *.pdf
