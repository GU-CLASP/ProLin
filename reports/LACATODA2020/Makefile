default:
	nix-shell --pure --run "make lacatoda.pdf"

%.pdf: %.tex
	latekmk $<

%.tex: %.lhs
	lhs2tex -o $@ $<

%.lhs: %.org
	emacs  --batch --eval="(message \"%s\" wofuytarnust)" $< -f org-latex-export-to-latex --kill
	mv -f $*.tex $@

