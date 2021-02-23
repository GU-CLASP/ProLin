default:
	nix-shell --pure --run "make tal.pdf"
reviews:
	nix-shell --pure --run "make reviews-202009.pdf"
slides:
	nix-shell --run "make slides-tal.pdf"

slides-%.pdf: slides-%.tex
	latexmk -pdf -xelatex $<

%.pdf: %.tex
	latexmk -pdf $<

%.tex: %.lhs
	lhs2TeX -o $@ $<

slides-%.lhs: slides-%.org
	emacs  --batch --eval="(load \"${MYEMACSLOAD}\")" $< -f org-beamer-export-to-latex --kill
	mv -f slides-tal.tex $@


%.lhs: %.org
	emacs  --batch --eval="(load \"${MYEMACSLOAD}\")" $< -f org-latex-export-to-latex --kill
	mv -f $*.tex $@
