(defsystem :planetwit
  :description "Gateway planet.lisp.org to Twitter"
  :serial t
  :depends-on (:hunchentoot
               :drakma
               :cl-oauth
               :cxml-stp
               :local-time)
  :components ((:file "planetwit")))
