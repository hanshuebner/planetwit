;; -*- Lisp -*-

(defpackage :faz-filter
  (:use :cl))

(in-package :faz-filter)

(defun postprocess-article (content)
  (ff:delete-nodes content "div/p[@class='ArtikelRelatedLinks']")
  (ff:delete-nodes content "div/ul[@class='RelatedLinkBox']")
  (ff:delete-nodes content "div/div[@class='ArtikelFooter']/following-sibling::*")
  (ff:delete-nodes content "div/div[@class='ArtikelFooter']")
  (ff:delete-nodes content "//iframe")
  (ff:delete-nodes content "//script")
  (ff:rewrite-urls content "src" "^" "http://faz.net/")
  (ff:delete-attributes content "onclick")
  (ff:delete-attributes content "itemprop")
  

(ff:define-feed faz-feuilleton
  :type :rss2.0
  :feed-url "http://www.faz.net/rss/aktuell/feuilleton/"
  :article-content-xpath "/html/body//div[@class='FAZArtikelContent']"
  :postprocess-article #'postprocess-article)
