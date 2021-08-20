default:
	nix-shell --pure --run "make reinact.tex"

%.pdf: %.tex
	latexmk -pdf $<

%.tex: %.lhs
	lhs2TeX -o $@ $<

%.lhs: %.org
	emacs  --batch --eval="(load \"${MYEMACSLOAD}\")" $< -f org-latex-export-to-latex --kill
	mv -f $*.tex $@
