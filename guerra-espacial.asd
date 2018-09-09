(asdf:defsystem #:guerra-espacial
  :description "Space War! clone"
  :author "José Miguel Ronquillo Rivera <jose@rufina.link>"
  :license  "GPL Ver. 3"
  :version "0.0.1"
  :serial t
  :depends-on (#:bordeaux-threads #:mcclim)
  :components ((:file "paquetes")
               (:file "estrella")
               (:file "guerra-espacial")))
