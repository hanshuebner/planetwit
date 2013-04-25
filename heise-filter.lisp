;; -*- Lisp -*-

(defpackage :heise-filter
  (:use :cl))

(in-package :heise-filter)

(defun postprocess-article (content)
  (ff:delete-matching-elements content "/p[@class='author_date']")
  (ff:delete-matching-elements content "/h1")
  (ff:rewrite-urls content "href" "^/" "http://www.heise.de/")
  (ff:rewrite-urls content "src" "^/" "http://www.heise.de/")
  (ff:rewrite-urls content "src" "^http://www.heise.de/resize/" "http://www.heise.de/"))

(defun blacklisted-author-p (name)
  (member name '("Peter Mühlbauer") :test #'string=))

(defun include-item (item)
  (not (blacklisted-author-p (ff:stripped-string-value (xpath:evaluate "author" item)))))

(defun preprocess-article-url (url)
  (ppcre:regex-replace "^http://www.heise.de" url "http://m.heise.de"))

(defun get-feed ()
  (ff:filtered-feed :type :atom
                    :feed-url "http://www.heise.de/tp/news-atom.xml"
                    :replacement-url "http://netzhansa.com/heise-atom"
                    :article-content-xpath "/html/body/div/div[@id='content']"
                    :preprocess-article-url #'preprocess-article-url
                    :include-item #'include-item
                    :postprocess-article #'postprocess-article))

(hunchentoot:define-easy-handler (heise-atom :uri "/heise-atom")
    ()
  (setf (hunchentoot:content-type*) "application/xml")
  (get-feed))
