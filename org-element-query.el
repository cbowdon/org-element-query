;; -*- lexical-binding: t -*-

(defun org-element-query (query &optional ast)
  "Select all contents of current buffer matching QUERY. QUERY is a list of symbols representing org-element types or properties.

If AST (abstract syntax tree) is non-nil, parse that rather than current buffer. AST should be a result of calling `org-element-parse-buffer'."
  (org-element-query--select query
			     (or ast (org-element-parse-buffer))))

(defun org-element-query--select (query element)
  (let ((s (car query))
	(rest (cdr query)))
    (cond ((not rest)
	   (cond ((org-element-query--propertyp s) (list (org-element-property s element)))
		 ((org-element-query--typep s element) (list element))
		 (t '())))
	  ((org-element-query--typep s element)
	   (cond ((org-element-query--propertyp (car rest))
		  (org-element-query--select rest element))
		 (t (org-element-query--flatmap
		     (lambda (e) (org-element-query--select rest e))
		     element)))))))

(defun org-element-query--propertyp (s)
  (and (symbolp s) (string-prefix-p ":" (symbol-name s))))

(defun org-element-query--map (fun element)
  (seq-map fun (org-element-contents element)))

(defun org-element-query--flatmap (fun element)
  (message (format "flatmap of %s" element))
  (apply 'seq-concatenate 'list
	 (org-element-query--map fun element)))

(defun org-element-query--typep (type element)
  (eq type (org-element-type element)))
