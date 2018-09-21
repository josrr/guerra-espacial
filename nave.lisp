(in-package #:guerra-espacial)

(declaim (inline toroidalizar actualiza-posicion-obj actualiza-direccion-obj
                 actualiza-mom-angular gravedad empuje-nave))

(defun hay-colision-p (pane)
  (declare (optimize (speed 3)))
  (loop for todos on (remove nil (espacio-objs pane) :key (lambda (o) (getf o :colisiona)))
     for obj1 = (car todos)
     for obj1-es-nave = (eq :nave (getf obj1 :tipo))
     for x1 double-float = (if obj1-es-nave  (getf obj1 :xm) (getf obj1 :x))
     and y1 double-float = (if obj1-es-nave (getf obj1 :ym) (getf obj1 :y))
     append (remove-duplicates (mapcan (lambda (obj2)
                                         (let* ((obj2-es-nave (eq :nave (getf obj2 :tipo)))
                                                (x2 (getf obj2 :x))
                                                (y2 (getf obj2 :y))
                                                (dx (abs (- (if obj2-es-nave (the double-float (getf obj2 :xm)) x2) x1)))
                                                (dy (abs (- (if obj2-es-nave (the double-float (getf obj2 :ym)) y2) y1))))
                                           (declare (type double-float x2 y2 dx dy))
                                           (when (and (< dx *radio-colision-1*)
                                                      (< dy *radio-colision-1*)
                                                      (< (+ dx dy) *radio-colision-2*))
                                             (list obj1 obj2))))
                                       (cdr todos)))))

(defun toroidalizar (obj)
  (declare (optimize (speed 3) (safety 0)))
  (let ((x (getf obj :x))
        (y (getf obj :y)))
    (declare (type double-float x y))
    (if (< x *min-x*)
        (setf (getf obj :x) (+ x *ancho-df*))
        (when (> x *max-x*)
          (setf (getf obj :x) (- x *ancho-df*))))
    (if (< y *min-y*)
        (setf (getf obj :y) (+ y *alto-df*))
        (when (> y *max-y*)
          (setf (getf obj :y) (- y *alto-df*))))))

(defun actualiza-posicion-obj (obj)
  (declare (optimize (speed 3) (safety 0)))
  (let* ((x0 (getf obj :x))
         (y0 (getf obj :y))
         (dx (getf obj :dx))
         (dy (getf obj :dy))
         (x (+ x0 (/ dx 8.0d0)))
         (y (+ y0 (/ dy 8.0d0))))
    (declare (type double-float x0 y0 dx dy))
    (setf (getf obj :y) y
          (getf obj :x) x)
    (toroidalizar obj)
    (values (getf obj :x) (getf obj :y))))

(defun explosion (pane obj)
  (declare (optimize (speed 3) (safety 0)))
  (multiple-value-bind (x y) (actualiza-posicion-obj obj)
    (declare (type double-float x y))
    (let ((x (+ 512.0d0 x))
          (y (- 512.0d0 y)))
      (loop for m fixnum from (floor (the fixnum (getf obj :tamaño)) 8) downto 1
         for factor double-float = (random (if (> m 96) 256.0d0 64.0d0))  do
           (draw-point* pane
                        (+ x (* factor (- (random 1.0d0) 0.5d0)))
                        (+ y (* factor (- (random 1.0d0) 0.5d0)))
                        :ink (if (zerop (random 2))
                                 +light-steel-blue+ +royalblue+
                                        ;+pink+ +deep-pink+
                                 )
                        :line-thickness 4))))
  (when (zerop (decf (the fixnum (getf obj :contador))))
    (setf (getf obj :func) nil
          (espacio-objs pane) (remove obj (espacio-objs pane)))))

(defun explota-obj (obj &optional (contador *duracion-explosion*))
  (when (and (eq :nave (getf obj :tipo)))
    (setf (getf obj :x) (getf obj :xm)
          (getf obj :y) (getf obj :ym)))
  (setf (getf obj :contador) (or contador *duracion-explosion*)
        (getf obj :dx) 0.0d0
        (getf obj :dy) 0.0d0
        (getf obj :colisiona) nil
        (getf obj :func) #'explosion))

(defun gravedad (obj)
  (declare (optimize (speed 3) (safety 0)))
  (let* ((es-nave (and (eq :nave (getf obj :tipo))))
         (x (getf obj (if es-nave :xm :x)))
         (y (getf obj (if es-nave :ym :y)))
         (t1 (+ (expt (* x *gravedad*) 2.0d0)
                (expt (* y *gravedad*) 2.0d0))))
    (declare (type double-float x y t1))
    (if (< t1 *radio-estrella*)
        (progn
          (explota-obj obj 32)
          (values))
        (let ((t1 (/ (* t1 (sqrt t1)) 2.0d0)))
          (values (/ (- x) t1)
                  (/ (- y) t1))))))

(defun empuje-nave (nave aceleracion)
  (declare (optimize (speed 3) (safety 0))
           (type double-float aceleracion))
  (values (/ (the double-float (getf nave :sen)) aceleracion)
          (/ (the double-float (getf nave :cos)) aceleracion)))

(defun actualiza-direccion-obj (obj am)
  (declare (optimize (speed 3) (safety 0))
           (type double-float am))
  (let* ((theta (getf obj :theta))
         (theta1 (+ theta am)))
    (declare (type double-float theta theta1))
    (setf (getf obj :theta)
          (if (> theta1 *2pi*) (- theta1 *2pi*)
              (if (< theta1 (- *2pi*)) (+ theta1 *2pi*)
                  theta1)))) )

(defun actualiza-mom-angular (nave am izq der)
  (declare (optimize (speed 3) (safety 0))
           (type double-float am))
  (if izq
      (incf am *aceleracion-angular-nave*)
      (when der
        (decf am *aceleracion-angular-nave*)))
  (setf (getf nave :mom-angular) am))

(defun mueve-nave (nave)
  (let* ((ctrls (getf nave :controles))
         (izq (member :izq ctrls))
         (der (member :der ctrls))
         (empuje (member :empuje ctrls)))
    (actualiza-direccion-obj nave
                             (actualiza-mom-angular nave
                                                    (getf nave :mom-angular)
                                                    izq der))
    (multiple-value-bind (bx by) (gravedad nave)
      (when bx
        (when empuje
          (multiple-value-bind (d-bx d-by) (empuje-nave nave *aceleracion-nave*)
            (incf by d-by)
            (decf bx d-bx)))
        (incf (getf nave :dy) by)
        (incf (getf nave :dx) bx)
        (actualiza-posicion-obj nave)))
    (values empuje izq der)))

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
         (xo (getf nave :x))
         (yo (getf nave :y))
         (x (+ 512.0d0 xo))
         (y (- 512.0d0 yo))
         (abs-paso (coerce (abs paso) 'double-float))
         (sen (getf nave :sen))
         (cos (getf nave :cos))
         (ssn (* abs-paso sen))
         (scn (* abs-paso cos))
         (ssm (if (minusp paso) (- ssn) ssn))
         (scm (if (minusp paso) (- scn) scn))
         (ssc (+ ssn scm))
         (csn (- ssn scm))
         (ssd (+ scn ssm))
         (csm (- scn ssm)))
    (declare (type double-float xo yo x y ssn scn ssm scm ssc csn ssd csm abs-paso angulo)
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
                ;;(when (eq (getf nave :nombre) :ot2) (log:info x y))
                (setf (getf nave :xm) (/ (+ (- x 512d0) xo) 2)
                      (getf nave :ym) (/ (+ (- 512d0 y) yo) 2))
                (return t)))))
    (when empuje (dibuja-gases-nave pane x y sen cos +darkorange+ +white+))
    (when der    (dibuja-gases-nave pane (+ x (* 11d0 cos)) (+ y (* -11d0 sen)) cos (- sen) +snow3+ +white+ 6))
    (when izq    (dibuja-gases-nave pane (+ x (* -11d0 cos)) (+ y (* 11d0 sen)) (- cos) sen +snow3+ +white+ 6))))

(defun maneja-torpedo (pane torpedo)
  (multiple-value-bind (bx by) (gravedad torpedo)
    (when bx
      (incf (getf torpedo :dy) by)
      (incf (getf torpedo :dx) bx)
      (actualiza-posicion-obj torpedo)))
  (let ((x (+ 512.0d0 (getf torpedo :x)))
        (y (- 512.0d0 (getf torpedo :y))))
    (draw-point* pane x y :ink +blue+ :line-thickness 7)
    (draw-point* pane x y :ink +white+ :line-thickness 4))
  (if (> (getf torpedo :contador) 0)
      (progn
        (when (and (null (getf torpedo :colisiona))
                   (= (- *duracion-torpedos* 3) (getf torpedo :contador)))
          (setf (getf torpedo :colisiona) t))
        (decf (getf torpedo :contador)))
      (setf (getf torpedo :contador) *duracion-explosion*
            (getf torpedo :func) #'explosion)))

(defun nuevo-torpedo (nave num)
  (declare (optimize (speed 3))
           (type fixnum num))
  (let* ((x (getf nave :x))
         (y (getf nave :y))
         (dx (getf nave :dx))
         (dy (getf nave :dy))
         (theta (getf nave :theta))
         (sen (sin theta))
         (cos (cos theta)))
    (declare (type double-float sen cos x y theta dx dy))
    (list :tipo :torpedo
          :nombre (alexandria:symbolicate 'torpedo-
                                          (getf nave :nombre)
                                          '- (princ-to-string num))
          :func #'maneja-torpedo
          :x x
          :y y
          :dx (+ dx (* -60.0d0 sen))
          :dy (+ dy (* 60.0d0 cos))
          :sen sen
          :cos cos
          :theta theta
          :colisiona nil
          :contador *duracion-torpedos*
          :tamaño *tamaño-torpedos*
          :nave nave)))

(defun dispara-torpedo (pane nave)
  (when (> (getf nave :torpedos) 0)
    (push (nuevo-torpedo nave (getf nave :torpedos))
          (espacio-objs pane))
    (decf (getf nave :torpedos))))

;;(setf (getf nave :controles) (remove :fuego (getf nave :controles) :count 1))
(defun maneja-nave (pane nave)
  (setf (getf nave :sen) (sin (getf nave :theta))
        (getf nave :cos) (cos (getf nave :theta)))
  (multiple-value-bind (empuje izq der) (mueve-nave nave)
    (dibuja-nave pane nave empuje izq der))
  (if (and (member :fuego (getf nave :controles))
           (zerop (getf nave :disparando)))
      (progn
        (setf (getf nave :disparando) 5)
        (dispara-torpedo pane nave))
      (when (> (getf nave :disparando) 0)
        (decf (getf nave :disparando)))))

(defun carga-naves (lista)
  (mapcar (lambda (datos)
            (let ((nave (cadr datos)))
              (list :tipo :nave
                    :nombre (car datos)
                    :func #'maneja-nave
                    :x (or (getf nave :x) 0.0d0)
                    :y (or (getf nave :y) 0.0d0)
                    :dx 0.0d0
                    :dy 0.0d0
                    :sen 0.0d0
                    :cos 0.0d0
                    :mom-angular 0.0d0
                    :theta (or (getf nave :theta) 0.0d0)
                    :vel-angular 0.0d0
                    :combustible 64
                    :torpedos *numero-de-torpedos*
                    :colisiona t
                    :contador 0
                    :controles nil
                    :tamaño *tamaño-nave*
                    :disparando 0
                    :xm (or (getf nave :x) 0.0d0)
                    :ym (or (getf nave :y) 0.0d0)
                    :desc (loop for palabra in (getf nave :forma) append
                               (loop for v across (format nil "~o" palabra) collect
                                    (- (char-code v) (char-code #\0)))))))
          lista))

(defun dame-nave (pane nombre)
  (find nombre (espacio-objs pane)
        :key (lambda (n) (getf n :nombre))))

(defun agrega-control-nave (gadget nombre-nave control)
  (let ((nave (dame-nave gadget nombre-nave)))
    (push control (getf nave :controles))))

(defun quita-control-nave (gadget nombre-nave control)
  (let ((nave (dame-nave gadget nombre-nave)))
    (setf (getf nave :controles)
          (remove control (getf nave :controles)))))
