;; -*- Lisp -*-

(defpackage :feeds
  (:use :cl :ff))

(in-package :feeds)

(define-feed :rss2.0 titanic "http://www.titanic-magazin.de/ich.war.bei.der.waffen.rss" 
  :article-content-xpath "/html/body/div[@id='page']//div[@class='tt_news-bodytext']"
  :process-article (lambda (content)
                     (delete-nodes content "div[@class='tt_news-category']/following-sibling::*")
                     (delete-nodes content "div[@class='tt_news-category']")
                     (rewrite-attributes content "src" "^" "http://www.titanic-magazin.de/")))

(define-feed :rss2.0 faz-feuilleton "http://www.faz.net/rss/aktuell/feuilleton/"
  :article-content-xpath "/html/body//div[@class='FAZArtikelContent']"
  :process-article (lambda (content)
                     (delete-nodes content "div/p[@class='ArtikelRelatedLinks']")
                     (delete-nodes content "div/ul[@class='RelatedLinkBox']")
                     (delete-nodes content "div/div[@class='ArtikelFooter']/following-sibling::*")
                     (delete-nodes content "div/div[@class='ArtikelFooter']")
                     (delete-nodes content "//iframe")
                     (delete-nodes content "//script")
                     (rewrite-attributes content "src" "^" "http://faz.net/")
                     (delete-attributes content "onclick")
                     (delete-attributes content "itemprop")))

(define-feed :atom telepolis "http://www.heise.de/tp/news-atom.xml"
  :article-content-xpath "/html/body/div/div[@id='content']"
  :preprocess-article-url (lambda (url)
                            (ppcre:regex-replace "^http://www.heise.de" url "http://m.heise.de"))
  :include-item (lambda (item)
                  (not (member (stripped-string-value (xpath:evaluate "author" item)) '("Peter Mühlbauer") :test #'string=)))
  :process-article (lambda (content)
                     (delete-nodes content "/p[@class='author_date']")
                     (delete-nodes content "/h1")
                     (rewrite-attributes content "href" "^/" "http://www.heise.de/")
                     (rewrite-attributes content "src" "^/" "http://www.heise.de/")
                     (rewrite-attributes content "src" "^http://www.heise.de/resize/" "http://www.heise.de/")))
