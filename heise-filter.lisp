;; -*- Lisp -*-

(defpackage :heise-filter
  (:use :cl))

(in-package :heise-filter)

(defun get-article (url)
  (xpath:with-namespaces ((nil "http://www.w3.org/1999/xhtml"))
    (let ((element (stp:copy (or (first (xpath:all-nodes (xpath:evaluate "/html/body/div/div[@id='content']"
                                                                         (ff:get-article url))))
                                 (error "could not find content div in ~S" url)))))
      (ff:delete-matching-elements element "/p[@class='author_date']")
      (ff:delete-matching-elements element "/h1")
      (stp:do-recursively (child element)
        (when (typep child 'stp:element)
          (ff:substitute-attribute-url child "href" "^/" "http://www.heise.de/")
          (ff:substitute-attribute-url child "src" "^/" "http://www.heise.de/")
          (ff:substitute-attribute-url child "src" "^http://www.heise.de/resize/" "http://www.heise.de/")))
      element)))

(defparameter *feed-url* "http://www.heise.de/tp/news-atom.xml")

(defun blacklisted-author-p (name)
  (member name '("Peter Mühlbauer") :test #'string=))

(defun make-article-content-element (heise-url)
  (handler-case
      (let ((element (stp:make-element "content" "http://www.w3.org/2005/Atom")))
        (setf (stp:attribute-value element "type") "html")
        (stp:append-child element (get-article (ppcre:regex-replace "^http://www.heise.de" heise-url "http://m.heise.de")))
        element)
    (error (e)
      (format t "could not get content for ~A:~%~A~%" heise-url e)
      nil)))

(defun filtered-feed ()
  (ff:with-article-cache-cleanup ()
    (let* ((feed (ff:get-feed *feed-url* "http://netzhansa.com/heise-atom"))
           (root (stp:first-child feed)))
      (xpath:with-namespaces ((nil "http://www.w3.org/2005/Atom"))
        (xpath:do-node-set (entry (xpath:evaluate "/feed/entry" feed))
          (cond
            ((blacklisted-author-p (ff:stripped-string-value (xpath:evaluate "author" entry)))
             (stp:delete-child entry root))
            (t
             (stp:with-attributes ((link "href")) (xpath:first-node (xpath:evaluate "link" entry))
               (alexandria:when-let (content (make-article-content-element link))
                 (stp:append-child entry content)))))))
      (stp:serialize feed (cxml:make-string-sink)))))

(hunchentoot:define-easy-handler (heise-atom :uri "/heise-atom")
    ()
  (setf (hunchentoot:content-type*) "application/xml")
  (filtered-feed))
