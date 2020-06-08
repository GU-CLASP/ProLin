with import (fetchTarball https://github.com/NixOS/nixpkgs-channels/archive/nixos-20.03.tar.gz) {};

let  
     myEmacs = emacsWithPackages (epkgs: with epkgs; [org-ref]);
     myEmacsConfig = writeText "default.el" ''
       (add-to-list 'load-path "${emacsPackages.org-ref}")
       (require 'org-ref)
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
                       soul
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
