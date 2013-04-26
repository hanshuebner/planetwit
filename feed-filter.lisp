;; -*- Lisp -*-

(defpackage :feed-filter
  (:use :cl)
  (:nicknames :ff)
  (:export #:define-feed
           #:delete-nodes
           #:delete-attributes
           #:rewrite-attributes))

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

(defun stripped-string-value (nodeset)
  (string-trim '(#\Space #\Tab #\Newline) (xpath:string-value nodeset)))

(defun make-entity-encoded-article-string (content)
  (remove-xml-preamble 
   (stp:serialize (stp:make-document (stp:copy content)) (cxml:make-string-sink))))

(defun get-article (url &key (article-content-xpath "/") (postprocess-article #'identity))
  (xpath:with-namespaces ((nil "http://www.w3.org/1999/xhtml"))
    (funcall postprocess-article
             (or (xpath:first-node (xpath:evaluate article-content-xpath
                                                   (chtml:parse (ppcre:regex-replace-all "\\s*(\\r|&#13;)\\n?" (request-article url) " ") (stp:make-builder))))
                 (error "could not find content div in ~S" url)))))

(defun filtered-feed (&key
                        type
                        feed-url
                        replacement-url
                        article-content-xpath
                        (preprocess-article-url #'identity)
                        (include-item #'identity)
                        (postprocess-article #'identity))
  (check-type type (member :atom :rss2.0))
  (ff:with-article-cache-cleanup ()
    (let ((feed (ff:get-feed feed-url replacement-url)))
      (xpath:with-namespaces (((unless (eql type :atom) "atom") "http://www.w3.org/2005/Atom"))
        (xpath:do-node-set (item (xpath:evaluate (ecase type
                                                   (:atom "/feed/entry")
                                                   (:rss2.0 "/rss/channel/item"))
                                                 feed))
          (if (funcall include-item item)
              (let* ((content-element-name (ecase type
                                             (:atom "content")
                                             (:rss2.0 "description")))
                     (description (or (xpath:first-node (xpath:evaluate content-element-name item))
                                      (let ((element (stp:make-element content-element-name (when (eql type :atom) "http://www.w3.org/2005/Atom"))))
                                        (stp:append-child item element)
                                        element)))
                     (article (get-article (funcall preprocess-article-url
                                                    (ecase type
                                                      (:atom (stp:attribute-value (xpath:first-node (xpath:evaluate "link" item)) "href"))
                                                      (:rss2.0 (xpath:string-value (xpath:evaluate "link" item)))))
                                           :article-content-xpath article-content-xpath
                                           :postprocess-article postprocess-article)))
                (stp:delete-children description)
                (stp:append-child description (ecase type
                                                (:atom
                                                 (let ((element (stp:make-element "content" "http://www.w3.org/2005/Atom")))
                                                   (setf (stp:attribute-value element "type") "html")
                                                   (stp:append-child element (stp:copy article))
                                                   element))
                                                (:rss2.0
                                                 (stp:make-text (make-entity-encoded-article-string article))))))
              (stp:delete-child item (stp:parent item))))
        (stp:serialize feed (cxml:make-string-sink))))))

(defun remove-xml-preamble (string)
  (cl-ppcre:regex-replace "^<\\?xml version=\"1\\.0\" encoding=\"UTF-8\"\\?>\\n" string ""))

(defun delete-nodes (content xpath)
  (dolist (node (xpath:all-nodes (xpath:evaluate xpath content)))
    (stp:delete-child node (stp:parent node)))
  content)

(defun rewrite-attributes (content attribute-name regexp replacement)
  (stp:do-recursively (child content)
    (when (typep child 'stp:element)
      (ff:substitute-attribute-url child attribute-name regexp replacement)))
  content)

(defmacro define-feed (name &rest args)
  `(hunchentoot:define-easy-handler (,name :uri ,(format nil "/feed/~(~A~)" name)) ()
     (filtered-feed :replacement-url ,(format nil "http://netzhansa.com/feed/~(~A~)" name)
                    ,@args)))
