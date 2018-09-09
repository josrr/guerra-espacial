(in-package #:guerra-espacial)

(declaim (type double-float *desp-x* *pausa*))
(defparameter *ancho* 1024)
(defparameter *alto* 1024)
(defparameter *guesp* nil)
(defparameter *pausa* 0.03d0)
(defparameter *desp-x* (/ (* *pausa* 8192) (* 24 60 60)))
(defparameter *bloqueo* (bt:make-lock "Bloqueo de espacio-x"))
(defparameter *ruta-del-sistema* (asdf:component-pathname (asdf:find-system 'guerra-espacial)))

(defun dibuja-estrellas (pane max-x)
  (with-bounding-rectangle* (x0 y0 x1 y1) (sheet-region pane)
    (climi::with-double-buffering ((pane x0 y0 x1 y1) (wtf-wtf-wtf))
      (declare (ignore wtf-wtf-wtf))
      (labels ((dibuja-estrella (x y m color)
                 (let ((r (- (+ 6 (random 1.125)) m))
                       (c (* 0.5 color)))
                   (draw-point* pane x y :ink (clim:make-rgb-color (* 0.4 c) c c) :line-thickness (* 2.0 r))
                   (draw-point* pane x y :ink (clim:make-rgb-color (* 0.4 color) color color) :line-thickness (* 1.0 r)))))
        (with-slots (y fondo estrellas) pane
          (draw-rectangle* pane x0 y0 x1 y1 :filled t :ink fondo)
          (let ((x (bt:with-lock-held (*bloqueo*) (espacio-x pane))))
            (loop with limite-derecho = (+ x x1)
               for punto in (remove-if (lambda (e)
                                         (let ((e-x (getf e :x)))
                                           (or (> e-x limite-derecho) (< e-x x))))
                                       (espacio-estrellas pane))
               do (dibuja-estrella (- (getf punto :x) x)
                                   (getf punto :y)
                                   (getf punto :magnitud)
                                   (/ (- 8.0 (getf punto :magnitud)) 8.0)))
            (when (or (> (+ x x1) max-x) (< (- max-x x) x1))
              (loop with inicio-x = (- max-x x)
                 with borde-x = (- x1 inicio-x)
                 for punto in (remove-if (lambda (e) (> (getf e :x) borde-x)) (espacio-estrellas pane))
                 initially (draw-line* pane inicio-x 0 inicio-x y1 :ink +gray5+ :line-thickness 1)
                 do (dibuja-estrella (+ (getf punto :x) inicio-x)
                                     (getf punto :y)
                                     (getf punto :magnitud)
                                     (/ (- 8.0 (getf punto :magnitud)) 8.0))))
            (bt:with-lock-held (*bloqueo*)
              (setf (espacio-x pane) (- x *desp-x*)))))))))

(defun inicia ()
  (let ((frame (make-application-frame 'guerra-espacial)))
    (setf *guesp* frame)
    (run-frame-top-level frame)
    (setf *guesp* nil)
    (let ((hilo (find "Animación" (bt:all-threads) :key #'bt:thread-name)))
      (when hilo (bt:destroy-thread hilo))))
  :name "Guerra Espacial")

(defun anima (frame)
  (lambda ()
    (loop with pane = (find-pane-named frame 'espacio-pane)
       while (eq :enabled (frame-state frame)) do
         (dibuja-estrellas pane *ancho-mapa-estelar*)
         (sleep *pausa*))))

(defun main ()
  (bt:make-thread #'inicia :name "guesp"))


(defclass espacio-pane (climi::never-repaint-background-mixin basic-gadget)
  ((x :initform (- 4096 *ancho*) :accessor espacio-x)
   (y :initform 0 :accessor espacio-y)
   (fondo :initform +black+ :reader espacio-color-de-fondo)
   (estrellas :initform (carga-estrellas) :reader espacio-estrellas)))

(defmethod compose-space ((pane espacio-pane) &key width height)
  (declare (ignore width height))
  (make-space-requirement :min-width *ancho*
                          :min-height *alto*
                          :width *ancho*
                          :height *alto*))

;;(defmethod handle-repaint ((pane espacio-pane) region) (declare (ignore region)) (dibuja-estrellas pane *ancho-mapa-estelar*))

(define-application-frame
    guerra-espacial () ()
    (:panes (espacio-pane (make-pane 'espacio-pane)))
    (:layouts (:default espacio-pane))
    (:menu-bar t))

(defmethod run-frame-top-level :before (frame &key &allow-other-keys)
  (bt:make-thread (anima frame) :name "Animación"))

(defmethod handle-event ((gadget espacio-pane) (evento key-press-event))
  (when *application-frame*
    (with-slots (escenario) *application-frame*
      (case (keyboard-event-key-name evento)
        ((:right) (bt:with-lock-held (*bloqueo*)
                    (let ((nueva-x (+ 25 (espacio-x gadget))))
                      (setf (espacio-x gadget) (if (< nueva-x *ancho-mapa-estelar*)
                                                   nueva-x 0)))))
        ((:left) (bt:with-lock-held (*bloqueo*)
                   (let ((nueva-x (- (espacio-x gadget) 25)))
                     (setf (espacio-x gadget) (if (minusp nueva-x)
                                                  (+ *ancho-mapa-estelar* nueva-x)
                                                  nueva-x)))))
        ((:Q :|q|) (execute-frame-command *application-frame* `(com-salir)))))))


;;;; Comandos
(define-guerra-espacial-command (com-salir :name "salir" :menu t)
    ()
  (frame-exit *application-frame*))

