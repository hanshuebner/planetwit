;; -*- Lisp -*-

(defpackage :faz-filter
  (:use :cl))

(in-package :faz-filter)

(defun postprocess-article (content)
  (ff:delete-nodes content "div/p[@class='ArtikelRelatedLinks']")
  (ff:delete-nodes content "div/ul[@class='RelatedLinkBox']")
  (ff:delete-nodes content "div/div[@class='ArtikelFooter']/following-sibling::*")
  (ff:delete-nodes content "div/div[@class='ArtikelFooter']"))

(defun get-feed ()
  (ff:filtered-feed :type :rss2.0
                    :feed-url "http://www.faz.net/rss/aktuell/feuilleton/"
                    :replacement-url "http://netzhansa.com/faz-feuilleton-rss"
                    :article-content-xpath "/html/body//div[@class='FAZArtikelContent']"
                    :postprocess-article #'postprocess-article))

(hunchentoot:define-easy-handler (faz-feuilleton-rss :uri "/faz-feuilleton-rss")
    ()
  (setf (hunchentoot:content-type*) "application/xml")
  (get-feed))
