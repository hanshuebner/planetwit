;; -*- Lisp -*-

(defpackage :titanic-filter
  (:use :cl))

(in-package :titanic-filter)

(defun postprocess-article (content)
  (ff:delete-nodes content "div[@class='tt_news-category']/following-sibling::*")
  (ff:delete-nodes content "div[@class='tt_news-category']")
  (ff:rewrite-urls content "src" "^" "http://www.titanic-magazin.de/"))

(ff:define-feed titanic
  :type :rss2.0
  :feed-url "http://www.titanic-magazin.de/ich.war.bei.der.waffen.rss"
  :article-content-xpath "/html/body/div[@id='page']//div[@class='tt_news-bodytext']"
  :postprocess-article #'postprocess-article)
