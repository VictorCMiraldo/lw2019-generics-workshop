LATEXENGINE=--latex-engine=xelatex

default:slides.pdf

verbose:slides.md
	pandoc -t beamer slides.md -o slides.pdf\
	  --slide-level=2\
	  $(LATEXENGINE)\
	  --verbose\
	  --filter=pandoc-citeproc
	  --template uu-metropolis/beamer-template.tex


slides.pdf: slides.md uu-metropolis/beamer-template.tex
	pandoc -t beamer slides.md -o slides.pdf\
	  --slide-level=2\
	  $(LATEXENGINE)\
	  --template uu-metropolis/beamer-template.tex

tex: slides.md
	pandoc -t beamer slides.md -o slides.tex\
	  --slide-level=2\
	  $(LATEXENGINE)\
	  --verbose\
	  --template uu-metropolis/beamer-template.tex

bib: tex references.bib
	xelatex slides.tex
	bibtex slides
	xelatex slides.tex
	xelatex slides.tex
	rm -f *.blg *.bbl *.aux *.log *.nav *.out *.snm *.toc *.vrb


clean:
	rm *.aux *.log *.nav *.out *.snm *.toc *.vrb
