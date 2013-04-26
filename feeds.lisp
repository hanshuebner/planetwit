;; -*- Lisp -*-

(defpackage :feeds
  (:use :cl :ff))

(in-package :feeds)

(define-feed :rss2.0 titanic "http://www.titanic-magazin.de/ich.war.bei.der.waffen.rss" 
  :article-xpath "/html/body/div[@id='page']//div[@class='tt_news-bodytext']"
  :process-article ((delete-nodes "div[@class='tt_news-category']/following-sibling::*")
                    (delete-nodes "div[@class='tt_news-category']")
                    (rewrite-attributes "src" "^" "http://www.titanic-magazin.de/")))

(define-feed :rss2.0 faz-feuilleton "http://www.faz.net/rss/aktuell/feuilleton/"
  :article-xpath "/html/body//div[@class='FAZArtikelContent']"
  :process-article ((delete-nodes "div/p[@class='ArtikelRelatedLinks']")
                    (delete-nodes "div/ul[@class='RelatedLinkBox']")
                    (delete-nodes "div/div[@class='ArtikelFooter']/following-sibling::*")
                    (delete-nodes "div/div[@class='ArtikelFooter']")
                    (delete-nodes "//iframe")
                    (delete-nodes "//script")
                    (rewrite-attributes "src" "^" "http://faz.net/")
                    (delete-attributes "onclick")
                    (delete-attributes "itemprop")))

(define-feed :atom telepolis "http://www.heise.de/tp/news-atom.xml"
  :article-xpath "/html/body/div/div[@id='content']"
  :preprocess-article-url (lambda (url)
                            (ppcre:regex-replace "^http://www.heise.de" url "http://m.heise.de"))
  :include-item (lambda (item)
                  (not (member (stripped-string-value (xpath:evaluate "author" item)) '("Peter Mühlbauer") :test #'string=)))
  :process-article ((delete-nodes "/p[@class='author_date']")
                    (delete-nodes "/h1")
                    (rewrite-attributes "href" "^/" "http://www.heise.de/")
                    (rewrite-attributes "src" "^/" "http://www.heise.de/")
                    (rewrite-attributes "src" "^http://www.heise.de/resize/" "http://www.heise.de/")))
