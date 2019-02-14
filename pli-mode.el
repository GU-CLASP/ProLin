(defvar pli-mode-hook nil)

;; syntax table for comments, same as for haskell-mode
(defvar pli-syntax-table
  (let ((st (make-syntax-table)))
       (modify-syntax-entry ?\{  "(}1nb" st)
       (modify-syntax-entry ?\}  "){4nb" st)
       (modify-syntax-entry ?-  "_ 123" st)
       (modify-syntax-entry ?\n ">" st)
       st))

(setq pli-operators-regexp (regexp-opt '(":" "::" "->" "-o" "[" "]" "(" ")") t))
(setq pli-constant-regexp "[A-Z][[:word:]]+")
(setq pli-def-regexp "^[a-z][[:word:]]+")

(setq pli-font-lock-keywords
      `((,pli-def-regexp . font-lock-function-name-face)
	(,pli-constant-regexp . font-lock-type-face)
	(,pli-operators-regexp . font-lock-variable-name-face)))

(define-derived-mode pli-mode prog-mode
  "pli mode"
  "Major mode for editing Linear Prolog files"
  
  :syntax-table pli-syntax-table

  (setq font-lock-defaults '(pli-font-lock-keywords))
  (setq mode-name "pli")
  (setq comment-start "--"))

(provide 'pli-mode)
