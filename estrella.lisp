(in-package #:guerra-espacial)

(defparameter *ancho-mapa-estelar* 8192)

(defun carga-estrellas ()
  (mapcan (lambda (grupo)
            (mapcar (lambda (e)
                      (list :x (- *ancho-mapa-estelar* (first e))
                            :y (+ 511 (- (second e)))
                            :nombre (fourth e)
                            :constelacion (fifth e)
                            :magnitud (car grupo)))
                    (cdr grupo)))
          (with-open-file (stream (merge-pathnames #P"./datos-estrellas.lisp"
                                                   *ruta-del-sistema*) :direction :input)
            (read stream))))

(defun estrellas-max-x (estrellas)
  (apply #'max (mapcar (lambda (e) (getf e :x)) estrellas)))
