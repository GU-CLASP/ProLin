(defun reverse-list-with-dolist (list)
  "Using dolist, reverse the order of LIST."
  (let (value)  ; make sure list starts empty
    (dolist (element list value)
      (setq value (cons element value)))))

(defun read-lines (filePath)
  "Return a list of lines of a file at filePath."
  (with-temp-buffer
    (insert-file-contents filePath)
    (split-string (buffer-string) "\n" t)))

(f-write-text
(string-join 
 (reverse-list-with-dolist
   (let ((xs)
	 (prevq nil))
     (dolist (x (read-lines "reviews-202009.org") xs)
       (if (string-match "^\\(> \\).*$" x)
	   (progn
             (if (not prevq)
		 (setq xs (cons "\n#+BEGIN_QUOTE" xs)
		       xs (cons (replace-match "" nil nil x 1) xs))
               (setq xs (cons (replace-match "" nil nil x 1) xs)))
             (setq prevq t))
         (progn
	   (if prevq
	       (setq xs (cons "#+END_QUOTE\n" xs)
		     xs (cons x xs)
		     prevq nil)
             (setq xs (cons x xs)))
           (setq prevq nil)) ))))
 "\n")
'utf-8
"./out.org")

        
        
        
	


