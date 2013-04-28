(defsystem :planetwit
  :description "Gateway planet.lisp.org to Twitter"
  :serial t
  :depends-on (:hunchentoot
               :drakma
               :cl-oauth
               :cxml-stp
               :closure-html
               :cl-html5-parser
               :local-time)
  :components ((:file "planetwit")
               (:file "html5-stp")
               (:file "feed-filter")))
