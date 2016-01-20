;; -*- lexical-binding: t -*-

(load-file "org-element-query.el")

(ert-deftest org-element-query--root ()
  "e.g. /org-data should return entire tree"
  (should
   (equal '((org-data nil))
	  (org-element-query '(org-data)
			     '(org-data nil))))
  (should
   (not
    (org-element-query '(headline)
		       '(org-data nil))))
  (should
   (equal '((org-data nil (headline nil)))
	  (org-element-query '(org-data)
			     '(org-data nil (headline nil))))))

(ert-deftest org-element-query--types ()
  "e.g. /org-data/headline should return all top-level headlines"
  (should
   (equal '((headline nil)
	    (headline nil))
	  (org-element-query '(org-data headline)
			     '(org-data nil
					(headline nil)
					(example-block nil)
					(headline nil)))))
  (should
   (equal '((src-block nil)
	    (src-block nil))
	  (org-element-query '(org-data headline src-block)
			     '(org-data nil
					(headline nil
						  (src-block nil)
						  (src-block nil))))))
  (should 
   (not
    (org-element-query '(org-data headline example-block)
		       '(org-data nil
				  (headline nil
					    (src-block nil)
					    (src-block nil)))))))

(ert-deftest org-element-query--properties ()
  "e.g. /org-data/headline/src-block:name should return the src-block names"
  (should
   (equal '("foo" "bar")
	  (org-element-query '(org-data headline (src-block :name))
			     '(org-data nil
					(headline nil
						  (src-block (:name "foo"))
						  (src-block (:name "bar")))))))
  (should
   (not
    (org-element-query '(org-data headline (example-block :name))
		       '(org-data nil
				  (headline nil
					    (src-block (:name "foo"))
					    (src-block (:name "bar")))))))
  (should
   (equal nil
	  (org-element-query '(org-data headline src-block :baz)
			     '(org-data nil
					(headline nil
						  (src-block (:name "foo"))
						  (src-block (:name "bar")))))))
  (should
   (equal 
    '("foo" "bar")
    (org-element-query
     '(org-data (headline :raw-value identity) section (src-block :value))
     '(org-data nil
		(headline (:raw-value "Lisp")
			  (section nil
				   (src-block (:value "foo"))
				   (src-block (:value "bar")))))))))

(ert-deftest org-element-query--predicates ()
  "e.g. /org-data/headline[:raw-value=foo*]/src-block should return src blocks under a headline like `foo'"
  (should
   (equal '((src-block (:name "foo"))
	    (src-block (:name "bar")))
	  (org-element-query
	   '(org-data
	     (headline :raw-value (lambda (x) (string-prefix-p "foo" x)))
	     src-block)
	   '(org-data nil
		      (headline (:raw-value "foobar")
				(src-block (:name "foo"))
				(src-block (:name "bar")))))))
  (should
   (not
    (org-element-query
     '(org-data
       (headline :raw-value (lambda (x) (string-prefix-p "foo" x)))
       src-block)
     '(org-data nil
		(headline (:raw-value "baz")
			  (src-block (:name "foo"))
			  (src-block (:name "bar"))))))))

(ert-deftest org-element-query--complex-structures ()
  "e.g. /org-data/headline[:raw-value=foo*]/src-block:name should return src block names under a headline like `foo', but not under other src-block names"
  (should
   (equal '("foo" "bar" "baz")
	  (org-element-query
	   '(org-data
	     (headline :raw-value (lambda (x) (string-prefix-p "foo" x)))
	     (src-block :name))
	   '(org-data nil
		      (headline (:raw-value "foobar")
				(src-block (:name "foo"))
				(src-block (:name "bar")))
		      (headline (:raw-value "foobarbaz")
				(src-block (:name "baz")))
		      (headline (:raw-value "nope")
				(src-block (:name "not me"))
				(src-block (:name "nor me")))
		      (src-block (:name "nor me!")))))))

(ert-deftest org-element-query--optional-element ()
  "Should be able to omit element argument"
  (setq org-file (find-file-noselect "test.org"))

  (should
   (equal '("foo" "bar")
	  (with-current-buffer org-file
	    (org-element-query
	     '(org-data
	       (headline :raw-value (lambda (x) (string= "Lisp" x)))
	       section
	       (src-block :name))))))

  (should
   (with-current-buffer org-file
     (equal 
      (org-element-query
       '(org-data
	 (headline :raw-value (lambda (x) (string= "Lisp" x)))
	 section
	 (src-block :name))
       (org-element-parse-buffer))
      (org-element-query
       '(org-data
	 (headline :raw-value (lambda (x) (string= "Lisp" x)))
	 section
	 (src-block :name)))))))


