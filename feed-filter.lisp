;; -*- Lisp -*-

(defpackage :feed-filter
  (:use :cl)
  (:nicknames :ff)
  (:export #:define-feed
           #:delete-nodes
           #:delete-attributes
           #:rewrite-attributes
           #:stripped-string-value
           #:start-server))

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
   (html5-p :initarg :html5-p
            :reader html5-p)
   (article-xpath :initarg :article-xpath
                  :reader article-xpath)
   (preprocess-article-url :initarg :preprocess-article-url
                           :reader preprocess-article-url)
   (include-item :initarg :include-item
                 :reader include-item)
   (process-article :initarg :process-article
                    :reader process-article))
  (:default-initargs
   :article-xpath "/"
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

(defvar *content*)

(defun get-article (feed url)
  (xpath:with-namespaces ((nil "http://www.w3.org/1999/xhtml"))
    (let* ((article-html-text (ppcre:regex-replace-all "\\s*(\\r|&#13;)\\n?" (request-article feed url) " "))
           (*content* (or (xpath:first-node (xpath:evaluate (article-xpath feed)
                                                            (if (html5-p feed)
                                                                (html5-stp:parse article-html-text)
                                                                (chtml:parse article-html-text (stp:make-builder)))))
                          (warn "could not find content div in ~S" url))))
      (when *content*
        (funcall (process-article feed)))
      *content*)))

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
      (alexandria:when-let (link (xpath:first-node (xpath:evaluate "//link[@rel='self']" feed-content)))
        (setf (stp:attribute-value link "href")
              (replacement-url feed))))
    (xpath:with-namespaces (((atom-namespace-alias feed) "http://www.w3.org/2005/Atom"))
      (xpath:do-node-set (item (xpath:evaluate (item-xpath feed) feed-content))
        (if (funcall (include-item feed) item)
            (alexandria:when-let (new-content (get-article feed (funcall (preprocess-article-url feed) (item-link feed item))))
              (let ((content-element (or (xpath:first-node (xpath:evaluate (content-element-name feed) item))
                                         (let ((element (stp:make-element (content-element-name feed) (namespace feed))))
                                           (stp:append-child item element)
                                           element))))
                (stp:delete-children content-element)
                (set-content feed content-element new-content)))
            (stp:delete-child item (stp:parent item))))
      (stp:serialize feed-content (cxml:make-string-sink)))))

;; Article content editing

(defun delete-nodes (xpath)
  (dolist (node (xpath:all-nodes (xpath:evaluate xpath *content*)))
    (stp:delete-child node (stp:parent node))))

(defun delete-attributes (attribute-name)
  (stp:do-recursively (child *content*)
    (when (typep child 'stp:element)
      (alexandria:when-let (attribute (stp:find-attribute-named child attribute-name))
        (stp:remove-attribute child attribute)))))

(defun rewrite-attributes (attribute-name regexp replacement)
  (stp:do-recursively (child *content*)
    (when (typep child 'stp:element)
      (substitute-attribute-url child attribute-name regexp replacement))))

(defvar *feeds*)

(defmacro define-feed (type
                       name
                       url
                       &key
                         html5-p
                         (article-xpath "/html/body")
                         (preprocess-article-url (lambda (url) url))
                         (include-item (lambda (item) (declare (ignore item)) t))
                         (process-article nil))
  (let ((name (string-downcase name)))
    (setf (gethash name *feeds*) (make-instance
                                  (ecase type
                                    (:atom 'atom-feed)
                                    (:rss2.0 'rss2.0-feed))
                                  :url url
                                  :replacement-url (format nil "http://netzhansa.com/feed/~(~A~)" name)
                                  :html5-p html5-p
                                  :article-xpath article-xpath
                                  :preprocess-article-url (compile nil preprocess-article-url)
                                  :include-item (compile nil include-item)
                                  :process-article (compile nil `(lambda () ,@process-article))))))

(defvar *users* (make-hash-table :test #'equal))

(defun load-feeds (&key (directory "users/"))
  (dolist (feed-definition-file (directory (merge-pathnames "*.lisp" directory)))
    (let* ((user-name (pathname-name feed-definition-file))
           (*package* (or (find-package (string-upcase user-name))
                          (make-package (string-upcase user-name)
                                        :use '(:cl :feed-filter))))
           ;; fixme: user feeds never deleted
           (*feeds* (or (gethash user-name *users*)
                        (setf (gethash user-name *users*) (make-hash-table :test #'equal)))))
      (load (compile-file feed-definition-file)))))

(defun dispatch-feed-handlers (request)
  (load-feeds)
  (ppcre:register-groups-bind (user-name feed-name) ("/feed/(.*)/(.*)$" (hunchentoot:script-name request))
    (alexandria:when-let (feeds (gethash user-name *users*))
      (alexandria:when-let (feed (gethash feed-name feeds))
        (setf (hunchentoot:content-type*) "application/xml")
        (lambda (&key) (filtered feed))))))

(pushnew 'dispatch-feed-handlers hunchentoot:*dispatch-table*)

(defun start-server (&key (port 9292))
  (hunchentoot:start (make-instance 'hunchentoot:easy-acceptor
                                    :port port
                                    :taskmaster (make-instance 'hunchentoot:single-threaded-taskmaster))))
