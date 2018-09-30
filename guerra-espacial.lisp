(in-package #:guerra-espacial)

(defun anima (frame)
  (lambda ()
    (loop with pane = (find-pane-named frame 'espacio-pane)
          while (bt:with-lock-held ((guesp-bloqueo frame)) (null (espacio-terminar pane)))
            initially (presentacion pane)
          do (bt:with-lock-held ((guesp-bloqueo frame))
               (unless (espacio-terminar pane)
                 (with-bounding-rectangle* (x0 y0 x1 y1) (sheet-region pane)
                   (climi::with-double-buffering ((pane x0 y0 x1 y1) (wtf-wtf-wtf))
                     (declare (ignore wtf-wtf-wtf))
                     (when (espacio-animacion-func pane)
                       (funcall (espacio-animacion-func pane) pane))))))
             (bt:thread-yield)
             (sleep *pausa*))))


(defun inicia ()
  (let* ((teclas (make-hash-table))
         (frame (make-application-frame 'guerra-espacial
                                        :teclas teclas)))
    (mapcar (lambda (nave)
              (dolist (op '(:izq :der :empuje :fuego :hiperespacio))
                (dolist (tecla (getf (cdr nave) op))
                  (setf (gethash tecla teclas) (list (car nave) op)))))
            *teclas*)
    (run-frame-top-level frame)
    (when (espacio-pixmap (find-pane-named frame 'espacio-pane))
      (clim:deallocate-pixmap (espacio-pixmap (find-pane-named frame 'espacio-pane))))
    (let ((hilo (guesp-hilo-animacion frame)))
      (when (and hilo (bt:thread-alive-p hilo)
                 (bt:destroy-thread hilo)))))
  :name "Guerra Espacial")

(defun salir ()
  #+sbcl (sb-ext:quit)
  #+ecl (ext:quit)
  #+clisp (ext:exit)
  #+ccl (ccl:quit)
  #-(or sbcl ecl clisp ccl) (cl-user::quit))

(defun main (&rest arguments)
  (declare (ignore arguments))
  (let ((hilo (bt:make-thread #'inicia :name "guesp")))
    (bt:join-thread hilo)
    (unless (find :swank *features*)
      (salir))))


(defclass espacio-pane (climi::never-repaint-background-mixin basic-gadget)
  ((x :initform (- 4096 *ancho*) :accessor espacio-x)
   (y :initform 0 :accessor espacio-y)
   (fondo :initform +black+
          :reader espacio-color-de-fondo)
   (pixmap :initform nil :accessor espacio-pixmap)
   (estrellas :initform *estrellas* :reader espacio-estrellas)
   (objetos :initform (carga-naves *naves*) :accessor espacio-objs)
   (num-cuadro :initform 1 :accessor espacio-num-cuadro)
   (animacion-func :initarg :animacion-func :initform nil :accessor espacio-animacion-func)
   (datos :initform (list) :accessor espacio-datos)
   (jugando :initform nil :accessor espacio-jugando)
   (terminar :initform nil :accessor espacio-terminar)))

(defmethod initialize-instance :after ((pane espacio-pane) &key contents)
  (declare (ignore contents))
  t)

(defmethod compose-space ((pane espacio-pane) &key width height)
  (declare (ignore width height))
  (make-space-requirement :min-width *ancho*
                          :min-height *alto*
                          :width *ancho*
                          :height *alto*
                          :max-width *ancho*
                          :max-height *alto*))

(define-application-frame guerra-espacial ()
  ((teclas :initarg :teclas :initform nil)
   (hilo-animacion :initform (bt:make-lock) :accessor guesp-hilo-animacion)
   (bloqueo :initform (bt:make-lock "Bloqueo para el frame") :accessor guesp-bloqueo))
  (:panes (espacio-pane (make-pane 'espacio-pane
                                   :animacion-func #'spacewar!)))
  (:layouts (:default espacio-pane))
  (:menu-bar t))

(defmethod run-frame-top-level :before (frame &key &allow-other-keys)
  (let* ((pane (find-pane-named frame 'espacio-pane))
         (pixmap (allocate-pixmap pane
                                  (bounding-rectangle-width (sheet-region pane))
                                  (bounding-rectangle-height (sheet-region pane)))))
    (setf (espacio-pixmap pane) pixmap)
    (setf (slot-value pixmap 'medium) (make-medium (port pane) pixmap)))
  (setf (guesp-hilo-animacion frame)
        (bt:make-thread (anima frame) :name "Animación")))


(defmethod handle-event ((gadget espacio-pane) (evento key-press-event))
  (when *application-frame*
    (with-slots (escenario teclas) *application-frame*
      (let* ((tecla (keyboard-event-key-name evento))
             (accion (gethash tecla teclas)))
        (if accion
            (agrega-control-nave gadget (car accion) (cadr accion))
            (case tecla
              ((:|Q| :|q|) (execute-frame-command *application-frame* `(com-salir)))
              ((:|N| :|n|) (execute-frame-command *application-frame* `(com-reiniciar)))
              (t (unless (espacio-jugando gadget)
                   (execute-frame-command *application-frame* `(com-guerra-espacial))))))))))

(defmethod handle-event ((gadget espacio-pane) (evento key-release-event))
  (when *application-frame*
    (with-slots (escenario teclas) *application-frame*
      (let* ((tecla (keyboard-event-key-name evento))
             (accion (gethash tecla teclas)))
        (when accion
          (quita-control-nave gadget (car accion) (cadr accion)))))))

;;;; Comandos

(define-guerra-espacial-command (com-guerra-espacial :name "Guerra Espacial" :menu t)
    ()
  (let ((espacio (find-pane-named *application-frame* 'espacio-pane)))
    (bt:with-lock-held ((guesp-bloqueo *application-frame*))
      (setf (espacio-jugando espacio) t
            (espacio-animacion-func espacio) #'spacewar!
            (espacio-num-cuadro espacio) 1))))

(define-guerra-espacial-command (com-minskytron :name "Minskytron" :menu t)
    ()
  (let ((espacio (find-pane-named *application-frame* 'espacio-pane)))
    (bt:with-lock-held ((guesp-bloqueo *application-frame*))
      (setf (espacio-jugando espacio) t
            (espacio-animacion-func espacio) #'minskytron
            (espacio-num-cuadro espacio) 1
            (espacio-datos espacio) nil)
      (draw-rectangle* (espacio-pixmap espacio) 0 0
                       (bounding-rectangle-width (sheet-region espacio))
                       (bounding-rectangle-height (sheet-region espacio))
                       :ink +black+))))

(define-guerra-espacial-command (com-munching-squares :name "Munching Squares" :menu t)
    ()
  (let ((espacio (find-pane-named *application-frame* 'espacio-pane)))
    (bt:with-lock-held ((guesp-bloqueo *application-frame*))
      (setf (espacio-jugando espacio) t
            (espacio-animacion-func espacio) #'munching-squares
            (espacio-num-cuadro espacio) 1))))

(define-guerra-espacial-command (com-reiniciar :name "Reiniciar" :menu t)
    ()
  (let ((espacio (find-pane-named *application-frame* 'espacio-pane)))
    (bt:with-lock-held ((guesp-bloqueo *application-frame*))
      (setf (espacio-jugando espacio) t
            (espacio-objs espacio) (carga-naves *naves*)
            (espacio-num-cuadro espacio) 1
            (espacio-datos espacio) nil)
      (draw-rectangle* (espacio-pixmap espacio) 0 0
                       (bounding-rectangle-width (sheet-region espacio))
                       (bounding-rectangle-height (sheet-region espacio))
                       :ink +black+))))

(define-guerra-espacial-command (com-salir :name "Salir" :menu t)
    ()
  (bt:with-lock-held ((guesp-bloqueo *application-frame*))
    (let ((espacio (find-pane-named *application-frame* 'espacio-pane)))
      (setf (espacio-terminar espacio) t)
      (frame-exit *application-frame*))))


(defun guerra-espacial-entry-point ()
  (apply 'main *command-line-arguments*))
