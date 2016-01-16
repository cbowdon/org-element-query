(defun org-element-query (path list-of-asts)
  "Select all contents of LIST-OF-ASTS matching PATH"
  (defun --subpath-match (p)
    (org-element--select-contents (car p) list-of-asts))
  (if (cadr path)
      (org-element-query (cdr path) (--subpath-match path))
    (--subpath-match path)))

(defun org-element--all-contents (list-of-asts)
  "Get a flat list of the contents of all ASTs in LIST-OF-ASTS"
  (apply 'seq-concatenate
	 'list
	 (seq-map 'org-element-contents list-of-asts)))

(defun org-element--select-contents (descriptor list-of-asts)
  "Selector all contents of ASTs in LIST-OF-ASTS matching DESCRIPTOR"
  (cond ((and (symbolp descriptor)
	      (string-prefix-p ":" (symbol-name descriptor)))
	 (seq-map (lambda (x) (org-element-property descriptor x)) list-of-asts))
	((functionp descriptor)
	 (message (format "func %s" (funcall descriptor 1)))
	 ;; WHY DOESN'T THIS WORK?
	 (org-element--filter-contents (lambda (x) (funcall descriptor x)) list-of-asts))
	(t (org-element--contents-of-type descriptor list-of-asts))))

(defun org-element--filter-contents (pred list-of-asts)
  "Filter all contents of ASTs in LIST-OF-ASTS by PRED"
  (seq-filter pred
	      (org-element--all-contents list-of-asts)))

(defun org-element--contents-of-type (type list-of-asts)
  "Select all contents of ASTs in LIST-OF-ASTS matching TYPE"
  (org-element--filter-contents
   (lambda (c) (eq type (org-element-type c)))
   list-of-asts))
