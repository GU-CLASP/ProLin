default: lacatoda.pdf

%.pdf: %.tex
	latekmk $<

%.tex: %.lhs
	nix-shell --pure --run "lhs2tex -o $@ $<"

%.lhs: %.org
	nix-shell --pure --run "emacs -q $< --batch -f org-latex-export-to-latex --kill"
	mv -f $*.tex $@

