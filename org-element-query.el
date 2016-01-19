;; -*- lexical-binding: t -*-

(defun org-element-query (query &optional ast)
  "Select all contents of current buffer matching QUERY. QUERY is a list of symbols representing org-element types or properties.

If AST (abstract syntax tree) is non-nil, parse that rather than current buffer. AST should be a result of calling `org-element-parse-buffer'."
  (org-element-query--internal query
			       (or ast (org-element-parse-buffer))))

(defun org-element-query--internal (query element)
  (let ((s (car query))
	(rest (cdr query)))
    ;; TODO is there a cleaner way?
    (cond ((not rest)
	   (cond ((org-element-query--propertyp s) (list (org-element-property s element)))
		 ((org-element-query--typep s element) (list element))
		 (t '())))
	  ((org-element-query--typep s element)
	   (cond ((org-element-query--propertyp (car rest))
		  (org-element-query--internal rest element))
		 (t (org-element-query--flatmap
		     (lambda (e) (org-element-query--internal rest e))
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

;; if it were xpath
;; /org-data/headline[:raw-value=Ex*]/section/src-block/:value

'(org-data
  (headline :raw-value (lambda (x) (string-prefix-p "Ex" x)))
  section
  (src-block :value))

;; type query =
;; | '(element-type)
;; | '(element-type . query)
;; | '((element-type property pred) . query)
;; | '((element-type property))
;; | '((element-type property pred)) TODO

(defun make-like-xpath (query)
  (pcase query
    (`((,type ,prop)) (list (format "/%s%s" type prop)))
    (`(,type . nil) (list (format "/%s" type)))
    (`((,type ,prop ,pred) . ,rest)
     (cons (format "/%s[%s=%s]" type prop pred)
	   (make-like-xpath rest)))
    (`(,type . ,rest)
     (cons (format "/%s" type)
	   (make-like-xpath rest)))
    (unknown (list (format "/unknown %s" unknown)))))

(defun oeq (query element)
  (pcase query
    (`((,type ,prop))
     (message (format "type prop %s" (org-element-type element)))
     (when (eq type (org-element-type element))
       (list (org-element-property prop element))))
    (`(,type . nil)
     (message (format "type nil %s" (org-element-type element)))
     (when (eq type (org-element-type element))
       (list element)))
    (`((,type ,prop ,pred) . ,rest)
     (message (format "type prop pred %s" (org-element-type element)))
     (when (and
	    (eq type (org-element-type element))
	    (funcall pred (org-element-property prop element)))
       (apply 'seq-concatenate 'list
	      (seq-map
	       (lambda (e) (oeq rest e))
	       (org-element-contents element)))))
    (`(,type . ,rest)
     (message (format "type rest %s" (org-element-type element)))
     (when (eq type (org-element-type element))
       (apply 'seq-concatenate 'list
	      (seq-map
	       (lambda (e) (oeq rest e))
	       (org-element-contents element)))))
    (unknown
     (message (format "unknown %s" unknown))
     '())))
