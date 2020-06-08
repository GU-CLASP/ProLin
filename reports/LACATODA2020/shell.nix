with import (fetchTarball https://github.com/NixOS/nixpkgs-channels/archive/nixos-20.03.tar.gz) {};

let  myEmacsConfig = writeText "default.el" ''
     (setq wofuytarnust "aorsitenwfyut")
     (require 'org-ref) 
'';
in stdenv.mkDerivation {
  name = "docsEnv";
  myEmacs = emacsWithPackages (epkgs: with epkgs; [
  (runCommand "default.el" {} ''
  mkdir -p $out/share/emacs/site-lisp
  cp ${myEmacsConfig} $out/share/emacs/site-lisp/defaut.el
  '')
  org-ref]);
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
