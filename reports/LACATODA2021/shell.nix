with import (fetchTarball https://github.com/NixOS/nixpkgs/archive/e9158eca70ae59e73fae23be5d13d3fa0cfc78b4.tar.gz)
{ overlays = [ (self: super: {})]; };
let
     myEmacs = emacsWithPackages (epkgs: with epkgs; [org-plus-contrib org-ref use-package epkgs.gnuplot]);
     myEmacsConfig = writeText "default.el" ''
       (add-to-list 'load-path "${emacsPackages.org-ref}")
       (require 'org-ref) ; broken?
       ;(message "Load path=%s" load-path)
       (org-babel-do-load-languages
        'org-babel-load-languages
         '((gnuplot . t)))
       (setq org-confirm-babel-evaluate nil)
       (setq org-export-use-babel t)
       (require 'ox-extra)
       (ox-extras-activate '(ignore-headlines))
     '';
in stdenv.mkDerivation {
  shellHook = ''
    export MYEMACSLOAD=${myEmacsConfig}
  '';
  name = "docsEnv";
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
                         enumitem
                       fontaxes
                       framed
                       fvextra
                       harvard
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
                       makecell
                       multirow
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
                       soul
                       stmaryrd
                         lazylist polytable # lhs2tex
                       tabulary
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
