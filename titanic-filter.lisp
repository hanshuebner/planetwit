;; -*- Lisp -*-

(defpackage :titanic-filter
  (:use :cl))

(in-package :titanic-filter)

(defparameter *rss-url* "http://www.titanic-magazin.de/ich.war.bei.der.waffen.rss")

(defun remove-xml-preamble (string)
  (cl-ppcre:regex-replace "^<\\?xml version=\"1\\.0\" encoding=\"UTF-8\"\\?>\\n" string ""))

(defun get-article (url)
  (xpath:with-namespaces ((nil "http://www.w3.org/1999/xhtml"))
    (let ((content (first (or (xpath:all-nodes (xpath:evaluate "/html/body/div[@id='page']//div[@class='tt_news-bodytext']"
                                                               (ff:get-article url)))
                              (error "could not find content div in ~S" url)))))
      (mapcar (alexandria:rcurry #'stp:delete-child content)
              (xpath:all-nodes (xpath:evaluate "div[@class='tt_news-category']/preceding-sibling::*/following-sibling::*" content)))
      (remove-xml-preamble 
       (stp:serialize (stp:make-document (stp:copy content)) (cxml:make-string-sink))))))

(defun get-entity-encoded-article (url)
  (remove-xml-preamble
   (with-output-to-string (s)
     (cxml:with-xml-output (cxml:make-character-stream-sink s)
       (cxml:text (get-article url))))))

(defun filtered-feed ()
  (ff:with-article-cache-cleanup ()
    (let ((feed (ff:get-feed *rss-url*)))
      (xpath:with-namespaces (("atom" "http://www.w3.org/2005/Atom"))
        (xpath:do-node-set (item (xpath:evaluate "/rss/channel/item" feed))
          (let ((description (xpath:first-node (xpath:evaluate "description" item))))
            (stp:delete-children description)
            (stp:append-child description (stp:make-text (get-article (xpath:string-value (xpath:evaluate "link" item))))))))
      (stp:serialize feed (cxml:make-string-sink)))))

(hunchentoot:define-easy-handler (heise-atom :uri "/titanic-rss")
    ()
  (setf (hunchentoot:content-type*) "application/xml")
  (filtered-feed))
