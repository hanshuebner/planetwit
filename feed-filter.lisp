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

(defparameter *log-timestamp-print-format* '((:year 4) #\- (:month 2) #\- (:day 2) #\Space (:hour 2) #\: (:min 2) #\: (:sec 2)))

(defvar *say-lock* (bt:make-lock))

(defun say (format &rest args)
  (bt:with-lock-held (*say-lock*)
    (fresh-line)
    (loop for prefix
            = (local-time:format-timestring nil (local-time:now) :format *log-timestamp-print-format*)
              then (format nil "~V,,,'.A" (length prefix) "\\")
          for line in (cl-ppcre:split #\Newline (apply #'format nil format args))
          unless (equal "" line)
            do (write-string prefix)
               (write-string ": ")
               (write-string line)
               (terpri))
    (finish-output)))

(defclass article-cache-entry ()
  ((url :initarg :url
        :reader url)
   (requested :initform (local-time:now)
              :reader requested)
   (last-used :initform (local-time:now)
              :reader last-used
              :accessor last-used%)
   (content :initarg :content
             :reader content)))

(defmethod print-object ((article-cache-entry article-cache-entry) stream)
  (print-unreadable-object (article-cache-entry stream :type t :identity t)
    (format stream "url ~A requested ~A last-used ~A"
            (url article-cache-entry)
            (requested article-cache-entry)
            (last-used article-cache-entry))))

(defmethod content :after ((article-cache-entry article-cache-entry))
  (setf (last-used% article-cache-entry) (local-time:now)))

(defvar *article-cache* (make-hash-table :test #'equal))

(defun cached-get-article (url)
  (content (or (gethash url *article-cache*)
               (setf (gethash url *article-cache*)
                     (make-instance 'article-cache-entry
                                    :url url
                                    :content (drakma:http-request url))))))

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
    (say "get-article ~A" url)
    (let* ((article-html-text (ppcre:regex-replace-all "\\s*(\\r|&#13;)\\n?" (cached-get-article url) " "))
           (*content* (or (xpath:first-node (xpath:evaluate (article-xpath feed)
                                                            (if (html5-p feed)
                                                                (html5-stp:parse article-html-text)
                                                                (chtml:parse article-html-text (stp:make-builder)))))
                          (warn "could not find content div in ~S" url))))
      (say "get-article process")
      (when *content*
        (funcall (process-article feed)))
      (say "get-article done")
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

(defun find-child (item element-name namespace)
  ;; xpath is overkill here
  (if namespace
      (xpath:with-namespaces ((nil namespace))
        (xpath:first-node (xpath:evaluate element-name item)))
      (xpath:first-node (xpath:evaluate element-name item))))

(defun ensure-child (item element-name namespace)
  (or (find-child item element-name namespace)
      (let ((element (stp:make-element element-name namespace)))
        (stp:append-child item element)
        element)))

(defgeneric content-element (feed item)
  (:method ((feed atom-feed) item)
    (ensure-child item "content" (namespace feed)))
  (:method ((feed rss2.0-feed) item)
    (or (find-child item "encoded" "http://purl.org/rss/1.0/modules/content/")
        (ensure-child item "description" nil))))

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
              (let ((content-element (content-element feed item)))
                (stp:delete-children content-element)
                (set-content feed content-element new-content)))
            (stp:delete-child item (stp:parent item))))
      (stp:serialize feed-content (cxml:make-string-sink)))))

;; Article content editing

(defun delete-nodes (xpath)
  (dolist (node (xpath:all-nodes (xpath:evaluate xpath *content*)))
    (stp:delete-child node (stp:parent node))))

(defun delete-attributes (attribute-regexp)
  (let ((scanner (ppcre:create-scanner attribute-regexp)))
    (stp:do-recursively (child *content*)
      (when (typep child 'stp:element)
        (dolist (attribute (stp:list-attributes child))
          (when (ppcre:scan scanner (stp:local-name attribute))
            (stp:remove-attribute child attribute)))))))

(defun rewrite-attributes (attribute-name regexp replacement)
  (stp:do-recursively (child *content*)
    (when (typep child 'stp:element)
      (substitute-attribute-url child attribute-name regexp replacement))))

(defvar *feeds*)
(defparameter *node-name* "jabberwock.netzhansa.com")

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
                                  :replacement-url (format nil "http://~A/feed/~(~A~)" *node-name* name)
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

(defun find-feed (user-name feed-name)
  (alexandria:when-let (feeds (gethash user-name *users*))
    (gethash feed-name feeds)))

(defun dispatch-feed-handlers (request)
  (load-feeds)
  (ppcre:register-groups-bind (user-name feed-name) ("/feed/(.*)/(.*)$" (hunchentoot:script-name request))
    (alexandria:when-let (feed (find-feed user-name feed-name))
      (setf (hunchentoot:content-type*) "application/xml")
      (lambda (&key) (filtered feed)))))

(pushnew 'dispatch-feed-handlers hunchentoot:*dispatch-table*)

(defvar *server* nil)

(defun start-server (&key (port 9292) (single-threaded-p nil))
  (when *server*
    (hunchentoot:stop *server*)
    (setf *server* nil))
  (setf *server* (hunchentoot:start (make-instance 'hunchentoot:easy-acceptor
                                                   :port port
                                                   :taskmaster (make-instance (if single-threaded-p
                                                                                  'hunchentoot:single-threaded-taskmaster
                                                                                  'hunchentoot:one-thread-per-connection-taskmaster))))))
