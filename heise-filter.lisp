;; -*- Lisp -*-

(defpackage :heise-filter
  (:use :cl))

(in-package :heise-filter)

(defvar *article-cache* (make-hash-table :test #'equal))
(defvar *next-cache*)

(defun request-article (url)
  (setf (gethash url *next-cache*)
        (or (gethash url *article-cache*)
            (drakma:http-request url))))

(defmacro with-article-cache-cleanup (() &body body)
  `(let ((*next-cache* (make-hash-table :test #'equal)))
     (prog1
         (progn ,@body)
       (setf *article-cache* *next-cache*))))

(defun get-article (url)
  (xpath:with-namespaces ((nil "http://www.w3.org/1999/xhtml"))
    (stp:copy (or (first (xpath:all-nodes (xpath:evaluate "/html/body/div/div[@id='content']"
                                                          (chtml:parse (request-article url) (stp:make-builder)))))
                  (error "could not find content div in ~S" url)))))

(defun get-feed ()
  (cxml:parse (drakma:http-request "http://www.heise.de/tp/news-atom.xml") (stp:make-builder)))

(defun blacklisted-author-p (name)
  (member name '("Peter Mühlbauer") :test #'string=))

(defun stripped-string-value (nodeset)
  (string-trim '(#\Space #\Tab #\Newline) (xpath:string-value nodeset)))

(defun make-article-content-element (heise-url)
  (ignore-errors
   (let ((element (stp:make-element "content" "http://www.w3.org/2005/Atom")))
     (setf (stp:attribute-value element "type") "html")
     (stp:append-child element (get-article (ppcre:regex-replace "^http://www.heise.de" heise-url "http://m.heise.de")))
     element)))

(defun filtered-feed ()
  (with-article-cache-cleanup ()
    (let* ((feed (get-feed))
           (root (stp:first-child feed)))
      (xpath:with-namespaces ((nil "http://www.w3.org/2005/Atom"))
        (setf (stp:attribute-value (xpath:first-node (xpath:evaluate "/feed/link[@rel='self']" feed)) "href")
              "http://netzhansa.com/heise-atom")
        (xpath:do-node-set (entry (xpath:evaluate "/feed/entry" feed))
          (cond
            ((blacklisted-author-p (stripped-string-value (xpath:evaluate "author" entry)))
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
