(in-package #:guerra-espacial)

(defun toroidalizar (obj)
  (let ((x (getf obj :x))
        (y (getf obj :y)))
    (if (< x *min-x*)
        (incf (getf obj :x) *ancho*)
        (when (> x *max-x*)
          (decf (getf obj :x) *ancho*)))
    (if (< y *min-y*)
        (incf (getf obj :y) *alto*)
        (when (> y *max-y*)
          (decf (getf obj :y) *alto*)))))

(defun explosion (pane nave)
  t)

(defun gravedad (nave)
  (let ((t1 (+ (expt (/ (getf nave :x) 8.0d0) 2.0d0)
               (expt (/ (getf nave :y) 8.0d0) 2.0d0))))
    (if (< t1 *radio-estrella*)
        (progn
          (setf (getf nave :dx) 0.0d0
                (getf nave :dx) 0.0d0
                (getf nave :colisiona) nil
                (getf nave :contador) 8
                (getf nave :func) #'explosion)
          (values))
        (let ((t1 (/ (* t1 (sqrt t1)) 2.0d0)))
          (values (/ (- (getf nave :x)) t1)
                  (/ (- (getf nave :y)) t1))))))

(defun empuje-nave (nave aceleracion)
  (values (/ (cos (getf nave :theta)) aceleracion)
          (/ (sin (getf nave :theta)) aceleracion)
          t))

(defun mueve-nave (nave)
  (let* ((am (getf nave :mom-angular))
         (ctrls (getf nave :controles))
         (izq (member :izq ctrls))
         (der (member :der ctrls))
         (empuje (member :empuje ctrls)))
    (if izq
        (incf am *aceleracion-angular-nave*)
        (when der
          (decf am *aceleracion-angular-nave*)))
    (setf (getf nave :mom-angular) am)
    (let ((theta1 (+ (getf nave :theta) am)))
      (setf (getf nave :theta)
            (if (> theta1 *2pi*) (- theta1 *2pi*)
                (if (< theta1 (- *2pi*)) (+ theta1 *2pi*)
                    theta1))))
    (multiple-value-bind (bx by) (gravedad nave)
      (unless bx (setf bx 0.0d0
                       by 0.0d0))
      (when empuje
        (multiple-value-bind (d-bx d-by) (empuje-nave nave *aceleracion-nave*)
          (incf by d-bx)
          (decf bx d-by)
          (setf empuje t)))
      (incf (getf nave :dy) by)
      (incf (getf nave :y) (/ (getf nave :dy) 8.0d0))
      (incf (getf nave :dx) bx)
      (incf (getf nave :x) (/ (getf nave :dx) 8.0d0)))
    (toroidalizar nave)
    (values empuje izq der)))

(defun dame-nave (pane nombre)
  (find nombre (espacio-naves pane) :key (lambda (n) (getf n :nombre))))

(defun dibuja-gases-nave (pane x y sen cos color-1 color-2 &optional (max-largo 16))
  (declare (optimize (speed 3) (safety 0))
           (type double-float x y sen cos)
           (type fixnum max-largo))
  (let ((largo (random max-largo))
        (sen (* 2 sen))
        (cos (* 2 cos)))
    (declare (type double-float sen cos)
             (type fixnum largo))
    (loop repeat largo
       for x double-float from x by sen
       for y double-float from y by cos
       for dx double-float = (- 1.0d0 (random 2.0d0)) do
         (draw-point* pane (+ dx x) y :ink color-1  :line-thickness 3)
         (draw-point* pane (- x dx) y :ink color-2 :line-thickness 1))))

(defun dibuja-nave (pane nave &optional empuje izq der (paso *paso-nave*) (ciclo 0))
  (declare (optimize (speed 3) (safety 0))
           (type fixnum ciclo))
  (let* ((angulo (getf nave :theta))
         (x (+ 512.0d0 (the double-float (getf nave :x))))
         (y (- 512.0d0 (the double-float (getf nave :y))))
         (abs-paso (coerce (abs paso) 'double-float))
         (sen (sin angulo))
         (cos (cos angulo))
         (ssn (* abs-paso sen))
         (scn (* abs-paso cos))
         (ssm (if (minusp paso) (- ssn) ssn))
         (scm (if (minusp paso) (- scn) scn))
         (ssc (+ ssn scm))
         (csn (- ssn scm))
         (ssd (+ scn ssm))
         (csm (- scn ssm)))
    (declare (type double-float x y ssn scn ssm scm ssc csn ssd csm abs-paso angulo)
             (type fixnum paso))
    (loop with guardada = nil
       for valor in (getf nave :desc) do
         (with-drawing-options (pane :ink +yellow+ :line-thickness 1 :line-cap-shape :round)
           (draw-point* pane (the fixnum (round x)) (the fixnum (round y)))
           (case valor
             ((0 1) (incf x ssn) (incf y scn) nil)
             (2     (incf x scm) (decf y ssm) nil)
             (3     (incf x ssc) (incf y csm) nil)
             (4     (decf x scm) (incf y ssm) nil)
             (5     (incf x csn) (incf y ssd) nil)
             (6 (if guardada
                    (setf x (car guardada)
                          y (cdr guardada)
                          guardada nil)
                    (setf guardada (cons x y))))
             (7 (when (zerop ciclo)
                  (dibuja-nave pane nave empuje izq der (- paso) 1))
                (return t)))))
    (when empuje (dibuja-gases-nave pane x y sen cos +darkorange+ +white+))
    (when der    (dibuja-gases-nave pane x y cos (- sen) +snow3+ +white+ 14))
    (when izq    (dibuja-gases-nave pane x y (- cos) sen +snow3+ +white+ 14))))

(defun maneja-nave (pane nave)
  (multiple-value-bind (empuje izq der) (mueve-nave nave)
    (dibuja-nave pane nave empuje izq der)))

(defun carga-naves (lista)
  (mapcar (lambda (datos)
            (let ((nave (cadr datos)))
              (list :nombre (car datos)
                    :func #'maneja-nave
                    :x (or (getf nave :x) 0.0d0)
                    :y (or (getf nave :y) 0.0d0)
                    :dx 0.0d0
                    :dy 0.0d0
                    :mom-angular 0.0d0
                    :theta (or (getf nave :theta) 0.0d0)
                    :vel-angular 0.0d0
                    :combustible 64
                    :torpedos 20
                    :colisiona t
                    :contador 0
                    :controles nil
                    :desc (loop for palabra in (getf nave :forma) append
                               (loop for v across (format nil "~o" palabra) collect
                                    (- (char-code v) (char-code #\0)))))))
          lista))

(defun agrega-control-nave (gadget nombre-nave control)
  (let ((nave (dame-nave gadget nombre-nave)))
    (push control (getf nave :controles))))
