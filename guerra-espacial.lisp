(in-package #:guerra-espacial)


(defun inicia ()
  (let* ((teclas (make-hash-table))
         (frame (make-application-frame 'guerra-espacial
                                        :teclas teclas)))
    (mapcar (lambda (nave)
              (dolist (op '(:izq :der :empuje :fuego :hiperespacio))
                (dolist (tecla (getf (cdr nave) op))
                  (setf (gethash tecla teclas) (list (car nave) op)))))
            *teclas*)
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
               (dibuja-estrellas pane)
               (dibuja-estrella pane (/ *ancho* 2) (/ *alto* 2))
               (loop for obj in (espacio-objs pane)
                  if (not (null (getf obj :func))) do
                    (funcall (getf obj :func) pane obj))
               (mapcar #'explota-obj (hay-colision-p pane)))))
         (sleep *pausa*))))

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
   (estrellas :initform *estrellas* :reader espacio-estrellas)
   (objetos :initform (carga-naves *naves*) :accessor espacio-objs)))

(defmethod compose-space ((pane espacio-pane) &key width height)
  (declare (ignore width height))
  (make-space-requirement :min-width *ancho*
                          :min-height *alto*
                          :width *ancho*
                          :height *alto*
                          :max-width *ancho*
                          :max-height *alto*))

(define-application-frame guerra-espacial ()
  ((teclas :initarg :teclas :initform nil))
  (:panes (espacio-pane (make-pane 'espacio-pane)))
  (:layouts (:default espacio-pane))
  (:menu-bar t))

(defmethod run-frame-top-level :before (frame &key &allow-other-keys)
  (bt:make-thread (anima frame) :name "Animación"))


(defmethod handle-event ((gadget espacio-pane) (evento key-press-event))
  (when *application-frame*
    (with-slots (escenario teclas) *application-frame*
      (let* ((tecla (keyboard-event-key-name evento))
             (accion (gethash tecla teclas)))
        (if accion
            (agrega-control-nave gadget (car accion) (cadr accion))
            (case tecla
              ((:|Q| :|q|) (execute-frame-command *application-frame* `(com-salir)))
              ((:|R| :|r|) (execute-frame-command *application-frame* `(com-reiniciar)))))))))

(defmethod handle-event ((gadget espacio-pane) (evento key-release-event))
  (when *application-frame*
    (with-slots (escenario teclas) *application-frame*
      (let* ((tecla (keyboard-event-key-name evento))
             (accion (gethash tecla teclas)))
        (when accion
          (quita-control-nave gadget (car accion) (cadr accion)))))))

;;;; Comandos
(define-guerra-espacial-command (com-salir :name "salir" :menu t)
    ()
  (frame-exit *application-frame*))

(define-guerra-espacial-command (com-reiniciar :name "Reiniciar" :menu t)
    ()
  (let ((espacio (find-pane-named *application-frame* 'espacio-pane)))
    (setf (espacio-objs espacio) (carga-naves *naves*))))


(defun guerra-espacial-entry-point ()
  (apply 'main *command-line-arguments*))
