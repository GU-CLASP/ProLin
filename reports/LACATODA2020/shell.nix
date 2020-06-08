with import (fetchTarball https://github.com/NixOS/nixpkgs-channels/archive/nixos-20.03.tar.gz) {};

stdenv.mkDerivation {
  name = "docsEnv";
  myEmacs = emacs;
  # myEmacs = (self.emacsPackagesNgGen self.emacs).emacsWithPackages (epkgs: with epkgs; []);
  buildInputs = [ haskellPackages.lhs2tex
                  # python3Packages.pygments
                  myEmacs
                  biber
                  # zip
                  (texlive.combine {
                       inherit (texlive)
                       algorithm2e
                       acmart
                       biblatex
                       boondox
                       collection-fontsrecommended
                       comment
                       cleveref
                       environ
                       fontaxes
                       framed
                       fvextra
                       ifplatform
                       ifsym
                       inconsolata
                       kastrup
                       latexmk
                       libertine
                       listings
                       lm
                       logreq
                       mathpartir
                       minted
                       mweights
                       ncclatex
                       ncctools
                       newtx
                       newtxsf
                       newtxtt
                       newunicodechar
                       prftree
                       relsize
                       scheme-small wrapfig marvosym wasysym
                       stmaryrd
                       lazylist polytable # lhs2tex
                       todonotes
                       totpages
                       trimspaces
                       thmtools
                       ucs
                       wasy cm-super unicode-math filehook lm-math capt-of
                       xargs
                       xstring ucharcat
                       xypic
                       xifthen
                       ifmtarg
                       ;
                     })
                ];
}
