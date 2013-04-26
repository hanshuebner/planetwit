;; -*- Lisp -*-

(defpackage :feed-filter
  (:use :cl)
  (:nicknames :ff)
  (:export #:define-feed
           #:delete-nodes
           #:delete-attributes
           #:rewrite-attributes
           #:stripped-string-value))

(in-package :feed-filter)

(defclass feed ()
  ((article-cache :initform (make-hash-table :test #'equal)
                  :accessor article-cache)
   (next-cache :initform nil
               :accessor next-cache)
   (url :initform (error "missing :url argument")
        :initarg :url
        :reader url)
   (replacement-url :initform (error "missing :replacement-url argument")
                    :initarg :replacement-url
                    :reader replacement-url)
   (article-content-xpath :initarg :article-content-xpath
                          :reader article-content-xpath)
   (preprocess-article-url :initarg :preprocess-article-url
                           :reader preprocess-article-url)
   (include-item :initarg :include-item
                 :reader include-item)
   (process-article :initarg :process-article
                    :reader process-article))
  (:default-initargs
   :article-content-xpath "/"
   :preprocess-article-url #'identity
   :include-item #'identity
   :process-article #'identity))

(defun request-article (feed url)
  (setf (gethash url (or (next-cache feed) (article-cache feed)))
        (or (gethash url (article-cache feed))
            (drakma:http-request url))))

(defgeneric filtered (feed)
  (:method :around ((feed feed))
    (setf (next-cache feed) (make-hash-table :test #'equal))
    (prog1
        (call-next-method)
      (setf (article-cache feed) (next-cache feed)
            (next-cache feed) nil))))

(defun delete-matching-elements (element xpath)
  (xpath:do-node-set (element (xpath:evaluate xpath element))
    (stp:delete-child element (stp:parent element))))

(defun substitute-attribute-url (element attribute-name from to)
  (alexandria:when-let (url (stp:attribute-value element attribute-name))
    (setf (stp:attribute-value element attribute-name) (ppcre:regex-replace from url to))))

(defun get-content (feed)
  (cxml:parse (drakma:http-request (url feed)) (stp:make-builder)))

(defun stripped-string-value (nodeset)
  (string-trim '(#\Space #\Tab #\Newline) (xpath:string-value nodeset)))

(defun remove-xml-preamble (string)
  (cl-ppcre:regex-replace "^<\\?xml version=\"1\\.0\" encoding=\"UTF-8\"\\?>\\n" string ""))

(defun make-entity-encoded-article-string (content)
  (remove-xml-preamble 
   (stp:serialize (stp:make-document (stp:copy content)) (cxml:make-string-sink))))

(defun get-article (feed url)
  (xpath:with-namespaces ((nil "http://www.w3.org/1999/xhtml"))
    (funcall (process-article feed)
             (or (xpath:first-node (xpath:evaluate (article-content-xpath feed)
                                                   (chtml:parse (ppcre:regex-replace-all "\\s*(\\r|&#13;)\\n?" (request-article feed url) " ") (stp:make-builder))))
                 (error "could not find content div in ~S" url)))))

(defclass atom-feed (feed)
  ())

(defclass rss2.0-feed (feed)
  ())

(defgeneric namespace (feed)
  (:method ((feed atom-feed))
    "http://www.w3.org/2005/Atom")
  (:method ((feed rss2.0-feed))
    nil))

(defgeneric atom-namespace-alias (feed)
  (:method ((feed t))
    "atom")
  (:method ((feed atom-feed))
    nil))

(defgeneric content-element-name (feed)
  (:method ((feed atom-feed))
    "content")
  (:method ((feed rss2.0-feed))
    "description"))

(defgeneric item-xpath (feed)
  (:method ((feed atom-feed))
    "/feed/entry")
  (:method ((feed rss2.0-feed))
    "/rss/channel/item"))

(defgeneric item-link (feed item)
  (:method ((feed atom-feed) item)
    (stp:attribute-value (xpath:first-node (xpath:evaluate "link" item)) "href"))
  (:method ((feed rss2.0-feed) item)
    (xpath:string-value (xpath:evaluate "link" item))))

(defgeneric set-content (feed content-element article)
  (:method ((feed atom-feed) content-element article)
    (setf (stp:attribute-value content-element "type") "html")
    (stp:append-child content-element (stp:copy article)))
  (:method ((feed rss2.0-feed) content-element article)
    (stp:append-child content-element (stp:make-text (make-entity-encoded-article-string article)))))

(defmethod filtered (feed)
  (let ((feed-content (get-content feed)))
    (xpath:with-namespaces ((nil "http://www.w3.org/2005/Atom"))
      (setf (stp:attribute-value (xpath:first-node (xpath:evaluate "//link[@rel='self']" feed-content)) "href")
            (replacement-url feed)))
    (xpath:with-namespaces (((atom-namespace-alias feed) "http://www.w3.org/2005/Atom"))
      (xpath:do-node-set (item (xpath:evaluate (item-xpath feed) feed-content))
        (if (funcall (include-item feed) item)
            (let ((content-element (or (xpath:first-node (xpath:evaluate (content-element-name feed) item))
                                       (let ((element (stp:make-element (content-element-name feed) (namespace feed))))
                                         (stp:append-child item element)
                                         element))))
              (stp:delete-children content-element)
              (set-content feed content-element (get-article feed (funcall (preprocess-article-url feed) (item-link feed item)))))
            (stp:delete-child item (stp:parent item))))
      (stp:serialize feed-content (cxml:make-string-sink)))))

(defun delete-nodes (content xpath)
  (dolist (node (xpath:all-nodes (xpath:evaluate xpath content)))
    (stp:delete-child node (stp:parent node)))
  content)

(defun delete-attributes (content attribute-name)
  (stp:do-recursively (child content)
    (when (typep child 'stp:element)
      (alexandria:when-let (attribute (stp:find-attribute-named child attribute-name))
        (stp:remove-attribute child attribute))))
  content)

(defun rewrite-attributes (content attribute-name regexp replacement)
  (stp:do-recursively (child content)
    (when (typep child 'stp:element)
      (substitute-attribute-url child attribute-name regexp replacement)))
  content)

(defvar *feeds* (make-hash-table :test #'equal))

(defmacro define-feed (type
                       name
                       url
                       &rest args
                       &key
                         article-content-xpath
                         preprocess-article-url
                         include-item
                         process-article)
  (declare (ignore article-content-xpath preprocess-article-url include-item process-article))
  (setf (gethash name *feeds*) (apply
                                #'make-instance
                                (ecase type
                                  (:atom 'atom-feed)
                                  (:rss2.0 'rss2.0-feed))
                                :allow-other-keys t
                                :url url
                                :replacement-url (format nil "http://netzhansa.com/feed/~(~A~)" name)
                                args)))

(defun handler ()
  (setf (hunchentoot:content-type*) "application/xml")
  #+(or)
  (filtered-feed :replacement-url ,
                 ,@args))
