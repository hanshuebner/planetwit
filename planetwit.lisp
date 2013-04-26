(defpackage :planetwit
  (:use :cl)
  (:export #:*twitter-access-token*
           #:main))

(in-package :planetwit)

(defun child-as-string (child-name element)
  (stp:string-value (first (stp:filter-children (stp:of-name child-name) element))))

(defun current-items ()
  (multiple-value-bind (content status headers uri stream close-stream-p reason)
      (drakma:http-request "http://planet.lisp.org/rss20.xml" :external-format-in :utf-8)
    (declare (ignore headers uri stream close-stream-p))
    (if (= 200 status)
        (mapcar (lambda (item)
                  (list :guid (child-as-string "guid" item)
                        :title (child-as-string "title" item)))
                (stp:filter-recursively (stp:of-name "item")
                                        (cxml:parse content
                                                    (stp:make-builder)
                                                    :entity-resolver (lambda (public-id system-id)
                                                                       (format t "resolve-entity public-id ~S system-id ~S~%" public-id system-id)
                                                                       (flex:make-in-memory-input-stream #())))))
        (warn "could not get current items from twitter (status ~A)" status))))

(defun shorten-url (url)
  (nth-value 0 (drakma:http-request (format nil "http://tinyurl.com/api-create.php?url=~A"
                                            (hunchentoot:url-encode url)))))

(defparameter *data-file-name* (asdf:system-relative-pathname :planetwit "planetwit.dat"))

(defun save-data (data)
  (with-open-file (f *data-file-name*
                    :direction :output
                    :if-exists :supersede
                    :external-format :utf-8)
    (with-standard-io-syntax
      (write data :stream f :readably t))))

(defun load-data ()
  (with-open-file (f *data-file-name*
                    :if-does-not-exist nil
                    :external-format :utf-8)
    (when f
      (read f))))

(defvar *twitter-access-token*)
(alexandria:when-let (file (probe-file "access-token.lisp"))
  (load file))

(defparameter *twitter-url* "https://api.twitter.com/1/statuses/update.xml")

(defun post-to-twitter (item)
  (format t "post-to-twitter: ~S~%" item)
  (destructuring-bind (&key guid title) item
    (let* ((tinyurl (shorten-url guid))
           #+(or)
           (status (format nil "~A ~A"
                           (subseq title 0
                                   (min (length title) (- 140 (length tinyurl) 1)))
                           tinyurl))
           (status (format nil "~A ~A"
                           (subseq title 0 (min (- 140 (length guid) 1) (length title)))
                           guid)))
      (format t "posting ~S~%" status)
      (multiple-value-bind (response status)
          (cl-oauth:access-protected-resource
           *twitter-url*
           *twitter-access-token*
           :request-method :post
           :user-parameters (list (cons "status" status)
                                  (cons "source" "planet_lisp")))
        (unless (= 200 status)
          (unless (stringp response)
            (setf response (flexi-streams:octets-to-string response)))
          (if (cl-ppcre:scan "Status is a duplicate" response)
              (warn "duplicate post skipped")
              (error "can't update twitter status: ~A ~A" status response)))))))

(defun poll-planet ()
  (let* ((old-data (load-data))
         (current-data (current-items))
         (new-items (set-difference current-data old-data
                                    :key (lambda (item) (getf item :guid))
                                    :test #'equal)))
    (when current-data
      (dolist (item new-items)
        (handler-case
            (post-to-twitter item)
          (error (e)
            (format t "could not post: ~A" e))))
      (save-data current-data))))

(defparameter *status-port* 3884)

(defvar *last-poll* (get-universal-time))

(defvar *errors* nil
  "List of errors that occured while polling or updating the status")
(defconstant +max-errors+ 10
  "After this many errors occured in succession, the status handler
  will produce an error so that the http monitor will notice the
  problem and send email.")

(hunchentoot:define-easy-handler (status :uri "/") ()
  (when (> (length *errors*) +max-errors+)
    (error "too many errors polling planet lisp:~%~{~A~%~}" *errors*))
  (setf (hunchentoot:content-type*) "text/plain")
  (format nil "Last Planet Lisp poll: ~A~%~@[Errors:~%~{~A~%~}~]"
          (drakma::render-cookie-date *last-poll*)
          *errors*))

(defun main ()
  (setf hunchentoot:*show-lisp-errors-p* t)
  (format t "starting hunchentoot~%")
  (let ((acceptor (hunchentoot:start (make-instance 'hunchentoot:easy-acceptor
                                                    :port *status-port*))))
    (unwind-protect
         (loop
           (setf *last-poll* (get-universal-time))
           (poll-planet)
           (sleep 60))
      (hunchentoot:stop acceptor))))
