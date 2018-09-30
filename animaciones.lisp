(in-package #:guerra-espacial)

(defun spacewar! (pane)
  (dibuja-estrellas pane)
  (dibuja-estrella pane (/ *ancho* 2) (/ *alto* 2))
  (loop for obj in (espacio-objs pane)
        if (not (null (getf obj :func))) do
          (funcall (getf obj :func) pane obj))
  (mapcar #'explota-obj (hay-colision-p pane)))

(defun munching-squares (pane)
  (declare (optimize (speed 3) (safety 0)))
  (draw-rectangle* pane 0 0 *ancho* *alto* :filled t :ink +black+)
  (loop with cuantos double-float = *ms-num-cuadros*
        with cuantos-1 fixnum = (round (1- cuantos))
        with total double-float = (* cuantos cuantos)
        with num-cuadro fixnum = (espacio-num-cuadro pane)
        with ancho double-float = (min (/ *ancho-df* cuantos) (/ *alto-df* cuantos))
        with ancho/2 double-float = (/ ancho 2.0d0)
        with cuantos-si double-float = 0.0d0
        for x from 0 to cuantos-1 and xmed double-float from ancho/2 by ancho do
          (loop for y from 0 to cuantos-1 and ymed double-float from ancho/2 by ancho
                if (< (logxor x y) num-cuadro) do
                  (draw-point* pane xmed ymed :ink +cyan+ :line-thickness ancho)
                  (incf cuantos-si))
        finally (if (>= cuantos-si total)
                    (setf (espacio-num-cuadro pane) 1)
                    (incf (the fixnum (espacio-num-cuadro pane))))))

(defun minskytron (pane)
  (let* ((datos (espacio-datos pane))
         (x (if datos (car datos) -19/24))
         (y (if datos (cadr datos) -1015/121))
         (δ 7381/5040)
         (ω 5040/7381)
         (ancho (bounding-rectangle-width (sheet-region pane)))
         (alto (bounding-rectangle-height (sheet-region pane))))
    (let ((pixmap (espacio-pixmap pane)))
      (when pixmap
        (when (= 1 (espacio-num-cuadro pane))
          (draw-rectangle* pixmap 0 0 ancho alto :filled t :ink +black+))
        (loop for i from 0 below 1000
              for px = (+ *ancho/2* x) and py = (- *alto/2* y)
              for color = (/ (log (incf (espacio-num-cuadro pane))) 255 10)
              if (and (>= px 0) (< px ancho) (>= py 0) (< py alto)) do
                (draw-point* pixmap px py
                             :ink (clim:make-rgb-color (random 1.0) 0.75
                                                       (if (> color 1) 1 color))
                             :line-thickness 1)
              end do
                (setf y (- y (floor x δ))
                      x (+ x (floor y ω))))
        (if (null (espacio-datos pane))
            (setf (espacio-datos pane) (list x y))
            (setf (car (espacio-datos pane)) x
                  (cadr (espacio-datos pane)) y))
        (copy-from-pixmap pixmap 0 0
                          (bounding-rectangle-width (sheet-region pane))
                          (bounding-rectangle-height (sheet-region pane))
                          pane 0 0)))))
