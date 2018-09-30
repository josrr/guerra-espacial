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
    (let ((x (+ *ancho-df/2* x))
          (y (- *alto-df/2* y)))
      (loop for m fixnum from (floor (the fixnum (getf obj :tamaño)) 8) downto 1
            for factor double-float = (random (if (> m 96) 256.0d0 64.0d0))  do
              (draw-point* pane
                           (+ x (* factor (- (random 1.0d0) 0.5d0)))
                           (+ y (* factor (- (random 1.0d0) 0.5d0)))
                           :ink (if (zerop (random 2))
                                    +light-steel-blue+ +royalblue+)
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
        (when (and empuje (> (getf nave :combustible) 0))
          (multiple-value-bind (d-bx d-by) (empuje-nave nave *aceleracion-nave*)
            (incf by d-by)
            (decf bx d-bx)))
        (incf (getf nave :dy) by)
        (incf (getf nave :dx) bx)
        (actualiza-posicion-obj nave)))
    (values empuje izq der)))

(defun dibuja-minskytron (pane num-puntos nave-x nave-y x y d w)
  (loop
    with desp-x = (+ nave-x *ancho-df/2*) and desp-y = (- *alto-df/2* nave-y)
    with i = x and j = y
    repeat num-puntos do
      (draw-point* pane (+ desp-x i) (- desp-y j) :ink +skyblue+ :line-thickness 4)
      (setf j (- j (floor i d))
            i (+ i (floor j w)))))

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
  (let* ((xo (getf nave :x))
         (yo (getf nave :y))
         (x (+ *ancho-df/2* xo))
         (y (- *alto-df/2* yo))
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
    (declare (type double-float xo yo x y ssn scn ssm scm ssc csn ssd csm abs-paso sen cos)
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
                 (setf (getf nave :xm) (/ (+ (- x *ancho-df/2*) xo) 2)
                       (getf nave :ym) (/ (+ (- *alto-df/2* y) yo) 2))
                 (return t)))))
    (when empuje (dibuja-gases-nave pane x y sen cos +darkorange+ +white+))
    (when der    (dibuja-gases-nave pane (+ x (* 11d0 cos)) (+ y (* -11d0 sen)) cos (- sen) +snow3+ +white+ 6))
    (when izq    (dibuja-gases-nave pane (+ x (* -11d0 cos)) (+ y (* 11d0 sen)) (- cos) sen +snow3+ +white+ 6))))

(defun maneja-torpedo (pane torpedo)
  (multiple-value-bind (bx by) (gravedad torpedo)
    (when bx (incf (getf torpedo :dy) by)
          (incf (getf torpedo :dx) bx)
          (let ((warpage (* 512.0d0 *torpedo-space-warpage*)))
            (declare (type double-float warpage))
            (incf (getf torpedo :dy) (/ (getf torpedo :x) warpage))
            (incf (getf torpedo :y) (/ (getf torpedo :dy) 8.0d0))
            (incf (getf torpedo :dx) (/ (getf torpedo :y) warpage))
            (incf (getf torpedo :x) (/ (getf torpedo :dx) 8.0d0)))
          (toroidalizar torpedo)))
  (let ((x (+ *ancho-df/2* (getf torpedo :x)))
        (y (- *alto-df/2* (getf torpedo :y))))
    (draw-point* pane x y :ink +blue+ :line-thickness 7)
    (draw-point* pane x y :ink +white+ :line-thickness 4))
  (if (> (getf torpedo :contador) 0)
      (progn
        (when (and (null (getf torpedo :colisiona))
                   (= (- *duracion-torpedos* 12) (getf torpedo :contador)))
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
         (sen (getf nave :sen))
         (cos (getf nave :cos)))
    (declare (type double-float sen cos x y dx dy))
    (list :tipo :torpedo
          :nombre (alexandria:symbolicate 'torpedo-
                                          (getf nave :nombre)
                                          '- (princ-to-string num))
          :func #'maneja-torpedo
          :x (- x (* 8.0d0 sen))
          :y (+ y (* 8.0d0 cos))
          :dx (- dx (* *torpedo-vel-inicial* sen))
          :dy (+ dy (* *torpedo-vel-inicial* cos))
          :sen sen
          :cos cos
          :colisiona nil
          :contador *duracion-torpedos*
          :tamaño *tamaño-torpedos*
          :nave nave)))

(defun sal-del-hiperespacio (pane nave)
  (if (zerop (getf nave :contador))
      (if (= (getf nave :hiperespacio) (random *hiperespacio-num-saltos*))
          (explota-obj nave)
          (setf (getf nave :hiperespacio-tiempo-enfriamiento) *hiperespacio-tiempo-enfriamiento*
                (getf nave :colisiona) t
                (getf nave :func) #'maneja-nave))
      (progn
        (draw-circle* pane
                      (+ *ancho-df/2* (getf nave :x)) (- *alto-df/2* (getf nave :y))
                      (/ *hiperespacio-tiempo-2* (getf nave :contador))
                      :filled nil
                      :ink +skyblue+
                      :line-thickness 2)
        (decf (getf nave :contador)))))

(defun hiperespacio (nave)
  (let* ((contador (getf nave :contador)))
    (lambda (pane nave)
      (if (> (getf nave :contador) 0)
          (let ((c (- contador (getf nave :contador))))
            (decf (getf nave :contador))
            (dibuja-minskytron pane c (getf nave :x) (getf nave :y) -12 12  -67 -1)
            (dibuja-minskytron pane c (getf nave :x) (getf nave :y) 12 12 1 (+ (floor c 4) 67)))
          (setf (getf nave :x) (- (random *ancho-df*) *ancho-df/2*)
                (getf nave :y) (- (random *ancho-df*) *alto-df/2*)
                (getf nave :xm) (getf nave :x)
                (getf nave :ym) (getf nave :y)
                (getf nave :theta) (random *2pi*)
                (getf nave :sen) (sin (getf nave :theta))
                (getf nave :cos) (cos (getf nave :theta))
                (getf nave :contador) *hiperespacio-tiempo-2*
                (getf nave :func) #'sal-del-hiperespacio)))))

(defun salta-al-hiperespacio (pane nave)
  (declare (ignore pane))
  (decf (getf nave :hiperespacio))
  (setf (getf nave :contador) *hiperespacio-tiempo-1*
        (getf nave :colisiona) nil
        (getf nave :func) (hiperespacio nave)))

(defun dispara-torpedo (pane nave)
  (when (> (getf nave :torpedos) 0)
    (push (nuevo-torpedo nave (getf nave :torpedos))
          (espacio-objs pane))
    (decf (getf nave :torpedos))))

(defun maneja-nave (pane nave)
  (setf (getf nave :sen) (sin (getf nave :theta))
        (getf nave :cos) (cos (getf nave :theta)))
  (multiple-value-bind (empuje izq der) (mueve-nave nave)
    (dibuja-nave pane nave (and empuje
                                (> (getf nave :combustible) 0))
                 izq der)
    (when (and empuje (> (getf nave :combustible) 0))
      (decf (getf nave :combustible))))

  (when (member :fuego (getf nave :controles))
    (if (zerop (getf nave :disparando))
        (progn
          (setf (getf nave :disparando) 32)
          (dispara-torpedo pane nave))))
  (when (> (getf nave :disparando) 0)
    (decf (getf nave :disparando)))
  (when (and (member :hiperespacio (getf nave :controles))
             (> (getf nave :hiperespacio) 0)
             (zerop (getf nave :hiperespacio-tiempo-enfriamiento)))
    (salta-al-hiperespacio pane nave))
  (when (> (getf nave :hiperespacio-tiempo-enfriamiento) 0)
    (decf (getf nave :hiperespacio-tiempo-enfriamiento))))

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
                    :combustible *combustible*
                    :torpedos *numero-de-torpedos*
                    :colisiona t
                    :contador 0
                    :controles nil
                    :hiperespacio *hiperespacio-num-saltos*
                    :hiperespacio-tiempo-enfriamiento 0
                    :tamaño *tamaño-nave*
                    :disparando 0
                    :xm (or (getf nave :x) 0.0d0)
                    :ym (or (getf nave :y) 0.0d0)
                    :desc (loop for palabra in (getf nave :forma)
                                append
                                (loop for v across (format nil "~o" palabra)
                                      collect (- (char-code v) (char-code #\0)))))))
          lista))

(defun dame-nave (pane nombre)
  (find nombre (espacio-objs pane)
        :key (lambda (n) (getf n :nombre))))

(defun agrega-control-nave (gadget nombre-nave control)
  (let ((nave (dame-nave gadget nombre-nave)))
    (bt:with-lock-held ((guesp-bloqueo *application-frame*))
      (push control (getf nave :controles)))))

(defun quita-control-nave (gadget nombre-nave control)
  (let ((nave (dame-nave gadget nombre-nave)))
    (bt:with-lock-held ((guesp-bloqueo *application-frame*))
      (setf (getf nave :controles)
            (remove control (getf nave :controles))))))
