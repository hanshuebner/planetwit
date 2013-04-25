;; -*- Lisp -*-

(defpackage :feed-filter
  (:use :cl)
  (:nicknames :ff)
  (:export #:request-article
           #:with-article-cache-cleanup
           #:delete-matching-elements
           #:substitute-attribute-url
           #:get-feed
           #:get-article
           #:stripped-string-value))

(in-package :feed-filter)

(defvar *article-cache* (make-hash-table :test #'equal))
(defvar *next-cache*)

(defun request-article (url)
  (setf (gethash url (if (boundp '*next-cache*) *next-cache* *article-cache*))
        (or (gethash url *article-cache*)
            (drakma:http-request url))))

(defmacro with-article-cache-cleanup (() &body body)
  `(let ((*next-cache* (make-hash-table :test #'equal)))
     (prog1
         (progn ,@body)
       (setf *article-cache* *next-cache*))))

(defun delete-matching-elements (element xpath)
  (xpath:do-node-set (element (xpath:evaluate xpath element))
    (stp:delete-child element (stp:parent element))))

(defun substitute-attribute-url (element attribute-name from to)
  (alexandria:when-let (url (stp:attribute-value element attribute-name))
    (setf (stp:attribute-value element attribute-name) (ppcre:regex-replace from url to))))

(defun get-feed (url new-location)
  (let ((feed (cxml:parse (drakma:http-request url) (stp:make-builder))))
    (xpath:with-namespaces ((nil "http://www.w3.org/2005/Atom"))
      (setf (stp:attribute-value (xpath:first-node (xpath:evaluate "//link[@rel='self']" feed)) "href")
            new-location))
    feed))

(defun get-article (url)
  (chtml:parse (ppcre:regex-replace-all "\\s*(\\r|&#13;)\\n?" (request-article url) " ") (stp:make-builder)))

(defun stripped-string-value (nodeset)
  (string-trim '(#\Space #\Tab #\Newline) (xpath:string-value nodeset)))

