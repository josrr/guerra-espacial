(in-package #:guerra-espacial)


(defun inicia ()
  (let ((frame (make-application-frame 'guerra-espacial)))
    (bt:with-lock-held (*bloqueo-guesp*) (setf *guesp* frame))
    (run-frame-top-level frame)
    (bt:with-lock-held (*bloqueo-guesp*) (setf *guesp* nil))
    (let ((hilo (find "Animación" (bt:all-threads) :key #'bt:thread-name)))
      (when hilo (bt:destroy-thread hilo))))
  :name "Guerra Espacial")

(defun anima (frame)
  (lambda ()
    (loop with pane = (find-pane-named frame 'espacio-pane)
       while (bt:with-lock-held (*bloqueo-guesp*) *guesp*) do
         (bt:with-lock-held (*bloqueo-guesp*)
           (with-bounding-rectangle* (x0 y0 x1 y1) (sheet-region pane)
             (climi::with-double-buffering ((pane x0 y0 x1 y1) (wtf-wtf-wtf))
               (declare (ignore wtf-wtf-wtf))
               (dibuja-estrellas pane *ancho-mapa-estelar*)
               (dibuja-estrella pane (/ *ancho* 2) (/ *alto* 2))
               (loop for obj in (espacio-naves pane) do
                    (funcall (getf obj :func) pane obj)
                    (setf (getf obj :controles) nil)))))
         (sleep *pausa*))))

(defun main ()
  (bt:make-thread #'inicia :name "guesp"))


(defclass espacio-pane (climi::never-repaint-background-mixin basic-gadget)
  ((x :initform (- 4096 *ancho*) :accessor espacio-x)
   (y :initform 0 :accessor espacio-y)
   (fondo :initform +black+
          :reader espacio-color-de-fondo)
   (estrellas :initform *estrellas* :reader espacio-estrellas)
   (naves :initform (carga-naves *naves*) :accessor espacio-naves)))

(defmethod compose-space ((pane espacio-pane) &key width height)
  (declare (ignore width height))
  (make-space-requirement :min-width *ancho*
                          :min-height *alto*
                          :width *ancho*
                          :height *alto*
                          :max-width *ancho*
                          :max-height *alto*))

;;(defmethod handle-repaint ((pane espacio-pane) region) (declare (ignore region)) (dibuja-estrellas pane *ancho-mapa-estelar*))

(define-application-frame
    guerra-espacial () ()
    (:panes (espacio-pane (make-pane 'espacio-pane)))
    (:layouts (:default espacio-pane))
    (:menu-bar t))

(defmethod run-frame-top-level :before (frame &key &allow-other-keys)
  (bt:make-thread (anima frame) :name "Animación"))

;;(defparameter *paso-angular* (/ pi 30.0d0))
(defmethod handle-event ((gadget espacio-pane) (evento key-press-event))
  (when *application-frame*
    (with-slots (escenario) *application-frame*
      (case (keyboard-event-key-name evento)
        ((:|a| :|A|) (agrega-control-nave gadget :ot2 :izq))
        ((:|d| :|D|) (agrega-control-nave gadget :ot2 :der))
        ((:|w| :|W|) (agrega-control-nave gadget :ot2 :empuje))
        ((:|s| :|S|) (agrega-control-nave gadget :ot2 :fuego))
        ((:|j| :|J|) (agrega-control-nave gadget :ot1 :izq))
        ((:|l| :|L|) (agrega-control-nave gadget :ot1 :der))
        ((:|k| :|K|) (agrega-control-nave gadget :ot1 :empuje))
        ((:|i| :|I|) (agrega-control-nave gadget :ot1 :fuego))
        ((:Q :|q|) (execute-frame-command *application-frame* `(com-salir)))))))

;;;; Comandos
(define-guerra-espacial-command (com-salir :name "salir" :menu t)
    ()
  (frame-exit *application-frame*))

(define-guerra-espacial-command (com-reiniciar :name "Reiniciar" :menu t)
    ()
  (let ((espacio (find-pane-named *application-frame* 'espacio-pane)))
    (setf (espacio-naves espacio) (carga-naves *naves*))))

