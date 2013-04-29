;; -*- Lisp -*-

(defpackage :html5-stp
  (:use :cl)
  (:export #:parse))

(in-package :html5-stp)

(defun node-to-stp (html5-node &optional parent)
  (ecase (html5-parser:node-type html5-node)
    (:doctype
     parent)
    (:document
     (let ((html-node (block find-html
                        (html5-parser:element-map-children (lambda (node)
                                                             (when (and (eql (html5-parser:node-type node) :element)
                                                                        (string= (html5-parser:node-name node) "html"))
                                                               (return-from find-html node)))
                                                           html5-node))))
       (unless html-node
         (error "no html child element in document root"))
       (stp:make-document (node-to-stp html-node))))
    (:fragment
     (error "fragment parsing not yet supported")
     #+(or)
     (html5-parser:element-map-children (alexandria:rcurry #'node-to-stp parent)
                                        html5-node))
    (:element
     (let ((element (stp:make-element (html5-parser:node-name html5-node) (html5-parser:node-namespace html5-node))))
       (html5-parser:element-map-attributes (lambda (name namespace value)
                                              (unless (ppcre:scan "^xmlns(:|$)" name)
                                                (when (ppcre:scan "^xml(:|$)" name)
                                                  (setf namespace "http://www.w3.org/XML/1998/namespace"))
                                                (setf (stp:attribute-value element name namespace) value)))
                                            html5-node)
       (html5-parser:element-map-children (alexandria:rcurry #'node-to-stp element) html5-node)
       (when parent
         (stp:append-child parent element))
       element))
    (:text
     (stp:append-child parent (stp:make-text (html5-parser:node-value html5-node)))
     parent)
    (:comment
     (stp:append-child parent (stp:make-comment (html5-parser:node-value html5-node)))
     parent)))

(defun parse (text)
  (node-to-stp (html5-parser:parse-html5 text :encoding :utf-8) (stp:make-element "dummy-root")))
