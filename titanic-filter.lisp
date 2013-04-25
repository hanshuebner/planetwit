;; -*- Lisp -*-

(defpackage :titanic-filter
  (:use :cl))

(in-package :titanic-filter)

(defun postprocess-article (content)
  (ff:delete-nodes content "div[@class='tt_news-category']/following-sibling::*")
  (ff:delete-nodes content "div[@class='tt_news-category']")
  (ff:rewrite-urls content "src" "^" "http://www.titanic-magazin.de/"))

(defun get-feed ()
  (ff:filtered-feed :type :rss2.0
                    :feed-url "http://www.titanic-magazin.de/ich.war.bei.der.waffen.rss"
                    :replacement-url "http://netzhansa.com/titanic-rss"
                    :article-content-xpath "/html/body/div[@id='page']//div[@class='tt_news-bodytext']"
                    :postprocess-article #'postprocess-article))

(hunchentoot:define-easy-handler (titanic-rss :uri "/titanic-rss")
    ()
  (setf (hunchentoot:content-type*) "application/xml")
  (get-feed))
