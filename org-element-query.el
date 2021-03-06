;; -*- lexical-binding: t -*-

(defun org-element-query (query &optional element)
  "Runs an XPath-like QUERY on an org-mode ELEMENT parse tree (i.e. output of `org-element-parse-buffer'). 
Returns a flat list of all selected items matching QUERY.

If ELEMENT is nil (and query starts with \"org-data\") then get ELEMENT by parsing current buffer.

Example queries:

1. To get all top-level headlines:

    '(org-data headline)

XPath equivalent: /org-data/headline

2. To get all top-level headlines starting with `foo':

    '(org-data
      (headline :raw-value (lambda (x) (string-prefix-p \"foo\" x))))

XPath(ish) equivalent: /org-data/headline[:raw-value=foo*]

You can use a shorthand for predicates to simplify that:

    '(org-data
      (headline :raw-value (string-prefix-p \"foo\")))

Of course you can also pass a function by its symbol:

    '(org-data
      (headline :raw-value starts-with-foo))

3. To get the names of any source blocks under top-level headlines starting with `foo':

    '(org-data
      (headline :raw-value (string-prefix-p \"foo\"))
      section
      (src-block :names))

XPath(ish) equivalent: /org-data/headline[:raw-value=foo*]/section/src-block:value

Query grammar:

type Query =
| '((Type Prop Pred) . Query)
| '((Type Prop) . nil)
| '(Type . Query)
| '(Type . nil)
where
  Type = org element type, see `org-element-all-elements'.
  Prop = org element property (e.g. name, caption)
  Pred = predicate on the property (i.e. an arity-one lambda, returning non-nil if satisfied)"

  (if (and (not element)
	   (eq 'org-data (car query)))
      (org-element-query query (org-element-parse-buffer))
    
    (pcase query

      (`((,type ,prop ,pred) . ,rest) 
       (when (and
	      (eq type (org-element-type element))
	      (org-element-query--apply-last pred (org-element-property prop element)))
	 (apply 'seq-concatenate 'list
		(seq-map (lambda (e) (org-element-query rest e))
			 (org-element-contents element)))))

      (`((,type ,prop) . nil) 
       (when (eq type (org-element-type element))
	 (list (org-element-property prop element))))

      (`(,type . ,rest?)
       (when (eq type (org-element-type element))
	 (if rest?
	     (apply 'seq-concatenate 'list
		    (seq-map (lambda (e) (org-element-query rest? e))
			     (org-element-contents element)))
	   (list element))))

      (_
       (error "Failed to parse query: %" query)))))

(defun org-element-query--apply-last (form x)
  "Enable shorthand notation of predicates. Note that form is always received quoted, hence not a macro."
  (pcase form
    ((and (pred listp)
	  (guard (or (eq 'lambda (car form))
		     (eq 'closure (car form)))))
     (funcall form x))
    (`(,f . ,args)
     (eval `(,f ,@args ,x)))
    (f
     (funcall f x))))
