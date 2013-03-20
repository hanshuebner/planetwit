(dolist (system '(:hunchentoot :drakma :cl-oauth :cxml-stp))
  (asdf:oos 'asdf:load-op system))

(defpackage :planetwit
  (:use :cl))

(in-package :planetwit)

(defun child-as-string (child-name element)
  (stp:string-value (first (stp:filter-children (stp:of-name child-name) element))))

(defun current-items ()
  (multiple-value-bind (content status headers uri stream close-stream-p reason)
      (drakma:http-request "http://planet.lisp.org/rss20.xml" :external-format-in :utf-8)
    (declare (ignore headers uri stream close-stream-p))
    (unless (= 200 status)
      (error "cannot poll planet lisp: status ~A reason ~A" status reason))
    (mapcar (lambda (item)
              (list :guid (child-as-string "guid" item)
                    :title (child-as-string "title" item)))
            (stp:filter-recursively (stp:of-name "item")
                                    (cxml:parse content
                                                (stp:make-builder)
                                                :entity-resolver (lambda (public-id system-id)
                                                                   (format t "resolve-entity public-id ~S system-id ~S~%" public-id system-id)
                                                                   (flex:make-in-memory-input-stream #())))))))

(defun shorten-url (url)
  (nth-value 0 (drakma:http-request (format nil "http://tinyurl.com/api-create.php?url=~A"
                                            (hunchentoot:url-encode url)))))

(defparameter *data-file-name* "planetwit.dat")

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

(defparameter *twitter-access-token*
  (make-instance 'cl-oauth:access-token
                 :consumer (make-instance 'cl-oauth:consumer-token
                                          :key "THMeDhHjrdeqsQdjuYppVQ"
                                          :secret "7A6wISNDVsKGXjh7Cz0oZKIonjuDVVO65Qh0D06VU0")
                 :key "16486287-lvIolgiqy2sT3qR3YusR8A2gA8pnkk55cCR4NkfVd"
                 :secret "VzySmgAAngnCXjhR3FhUfljJj6CaYA7dIK9JYazJNs"))
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
    (dolist (item new-items)
      (handler-case
          (post-to-twitter item)
        (error (e)
          (format t "could not post: ~A" e))))
    (save-data current-data)
    t))

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
  (let ((acceptor (hunchentoot:start (make-instance 'hunchentoot:acceptor
                                                    :port *status-port*))))
    (unwind-protect
         (loop
            (setf *last-poll* (get-universal-time))
            (handler-case
                (progn
                  (poll-planet)
                  (setf *errors* nil))
              (error (e)
                (push (princ-to-string e) *errors*)
                (format t "Error updating: ~A~%" e)))
            (sleep 60))
      (hunchentoot:stop acceptor))))