(in-package #:cl-user)

(defpackage #:guerra-espacial
  (:nicknames :guesp)
  (:use #:clim
        #:clim-lisp
        #:mcclim-raster-image
        #:uiop)
  (:export #:main
           #:guerra-espacial-entry-point))
