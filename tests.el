(load-file "org-element-query.el")

(ert-deftest org-element-query-test ()
  "Demonstrates simple path query extracting the name property of source blocks"
  (should
   (equal 2
	  (seq-length
	   (with-current-buffer "test.org"
	     (org-element-query
	      '(headline section src-block)
	      (list (org-element-parse-buffer)))))))
  (should
   (equal '("foo" "bar")
	  (with-current-buffer "test.org"
	    (org-element-query
	     '(headline section src-block :name)
	     (list (org-element-parse-buffer)))))))
