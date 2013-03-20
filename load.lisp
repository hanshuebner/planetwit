;; -*- Lisp -*-

(defpackage :load
  (:use :cl))

(in-package :load)

(load "~/quicklisp/setup.lisp")
(push (make-pathname :name nil :type nil :version nil :defaults *load-pathname*) asdf:*central-registry*)

(ql:quickload :planetwit)
