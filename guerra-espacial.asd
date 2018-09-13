(asdf:defsystem #:guerra-espacial
  :description "Spacewar! clone"
  :author "José Miguel Ronquillo Rivera <jose@rufina.link>"
  :license  "GPL Ver. 3"
  :version "0.0.1"
  :serial t
  :depends-on (#:bordeaux-threads #:mcclim #:mcclim-raster-image)
  :components ((:file "paquetes")
               (:file "parametros")
               (:file "nave")
               (:file "estrella")
               (:file "datos-naves")
               (:file "datos-estrellas")
               (:file "guerra-espacial")))
