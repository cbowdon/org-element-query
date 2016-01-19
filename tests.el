;; -*- lexical-binding: t -*-

(load-file "org-element-query.el")

(setq org-file (find-file-noselect "test.org"))

(ert-deftest org-element-query--typep ()
  "Type predicate should return t or nil"
  (with-current-buffer org-file
    (let ((data (org-element-parse-buffer)))
      (should
       (org-element-query--typep 'org-data data))
      (should
       (not (org-element-query--typep 'src-block data))))))

(ert-deftest org-element-query--propertyp ()
  "Property predicate should only pick up symbols like `:this'"
  (should (not (org-element-query--propertyp ":no")))
  (should (not (org-element-query--propertyp 'no)))
  (should (org-element-query--propertyp :no)))

(ert-deftest org-element-query--map ()
  "Should apply function to all contents of element"
  (should (not (org-element-query--map 'identity '())))
  (should
   (equal '(1 1 1)
	  (org-element-query--map (lambda (e) '1)
				  '(org-data nil
					     (headline nil)
					     (headline nil)
					     (headline nil)))))
  (should
   (equal '(headline headline headline)
	  (org-element-query--map 'org-element-type
				  '(org-data nil
					     (headline nil)
					     (headline nil)
					     (headline nil))))))

(ert-deftest org-element-query--flatmap ()
  "Should apply function to all children of element and flatten results"
  (should (not (org-element-query--flatmap 'identity '())))
  (should
   (equal '((src-block nil)
	    (src-block nil)
	    (example-block nil))
	  (org-element-query--flatmap
	   'org-element-contents
	   '(org-data nil
		      (headline nil (src-block nil) (src-block nil))
		      (headline nil (example-block nil))
		      (headline nil))))))

(ert-deftest org-element-query--internal ()
  "Should return query results"
  (should
   (equal '((org-data nil))
	  (org-element-query--internal '(org-data)
				       '(org-data nil))))
  (should
   (equal '((src-block nil)
	    (src-block nil))
	  (org-element-query--internal '(org-data src-block)
				       '(org-data nil
						  (src-block nil)
						  (src-block nil)))))
  (should 
   (not
    (org-element-query--internal '(org-data headline example-block)
				 '(org-data nil
					    (headline nil
						      (src-block nil)
						      (src-block nil))))))
  (should
   (equal '((src-block nil)
	    (src-block nil))
	  (org-element-query--internal '(org-data headline src-block)
				       '(org-data nil
						  (headline nil
							    (src-block nil)
							    (src-block nil))))))
  (should
   (equal '("foo" "bar")
	  (org-element-query--internal '(org-data headline src-block :name)
				       '(org-data nil
						  (headline nil
							    (src-block (:name "foo"))
							    (src-block (:name "bar")))))))
  (should
   (equal '(nil nil)
	  (org-element-query--internal '(org-data headline src-block :baz)
				       '(org-data nil
						  (headline nil
							    (src-block (:name "foo"))
							    (src-block (:name "bar"))))))))

(ert-deftest org-element-query-can-parse-test-buffer ()
  "Demonstrates simple path query extracting the name property of source blocks"
  (with-current-buffer org-file
    (should
     (eq nil (org-element-query '())))
    (should
     (equal 2
	    (seq-length
	     (org-element-query '(org-data headline section src-block)))))
    (should
     (equal '("foo" "bar")
	    (org-element-query '(org-data headline section src-block :name))))))

(ert-deftest org-element-query-can-provide-ast ()
  "Should be able to provide the data to parse for efficiency."
  (should
   (equal '("foo" "bar")
	  (org-element-query '(headline section src-block :name)
			     (with-current-buffer org-file
			       (car
				(org-element-contents
				 (org-element-parse-buffer))))))))

(ert-deftest make-like-xpath ()
  "Matching correctly"
  (should
   (equal 
    "/org-data/headline[:raw-value=identity]/section/src-block:value"
    (string-join
     (make-like-xpath
      '(org-data
	(headline :raw-value identity)
	section
	(src-block :value)))))))

(ert-deftest oeq ()
  "oeq"
  (should
   (equal 
    '("foo" "bar")
     (oeq
      '(org-data (headline :raw-value identity) section (src-block :value))
      '(org-data nil
		 (headline (:raw-value "Lisp")
			   (section nil
				    (src-block (:value "foo"))
				    (src-block (:value "bar")))))))))
