(defsystem :planetwit
  :description "Gateway planet.lisp.org to Twitter"
  :serial t
  :depends-on (:hunchentoot
               :drakma
               :cl-oauth
               :cxml-stp
               :closure-html)
  :components ((:file "planetwit")
               (:file "feed-filter")
               (:file "heise-filter")
               (:file "titanic-filter")
               (:file "faz-filter")))
