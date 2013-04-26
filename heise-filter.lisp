;; -*- Lisp -*-

(defpackage :heise-filter
  (:use :cl))

(in-package :heise-filter)

(defun postprocess-article (content)
  (ff:delete-matching-elements content "/p[@class='author_date']")
  (ff:delete-matching-elements content "/h1")
  (ff:rewrite-attributes content "href" "^/" "http://www.heise.de/")
  (ff:rewrite-attributes content "src" "^/" "http://www.heise.de/")
  (ff:rewrite-attributes content "src" "^http://www.heise.de/resize/" "http://www.heise.de/"))

(defun blacklisted-author-p (name)
  (member name '("Peter Mühlbauer") :test #'string=))

(defun include-item (item)
  (not (blacklisted-author-p (ff:stripped-string-value (xpath:evaluate "author" item)))))

(defun preprocess-article-url (url)
  (ppcre:regex-replace "^http://www.heise.de" url "http://m.heise.de"))

(ff:define-feed telepolis
  :type :atom
  :feed-url "http://www.heise.de/tp/news-atom.xml"
  :article-content-xpath "/html/body/div/div[@id='content']"
  :preprocess-article-url #'preprocess-article-url
  :include-item #'include-item
  :postprocess-article #'postprocess-article)

