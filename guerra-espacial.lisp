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
     for x from 0 to cuantos-1 and xa double-float from ancho/2 by ancho do
       (loop for y from 0 to cuantos-1 and ya double-float from ancho/2 by ancho
          if (< (logxor x y) num-cuadro) do
            (draw-point* pane xa ya :ink +cyan+ :line-thickness ancho)
            (incf cuantos-si))
     finally (if (= cuantos-si total)
                 (setf (espacio-num-cuadro pane) 0)
                 (incf (espacio-num-cuadro pane)))))

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
        (loop for i from 0 below 1000
           for px = (+ 512 x) and py = (- 512 y)
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

(defun anima (frame)
  (lambda ()
    (loop with pane = (find-pane-named frame 'espacio-pane)
       while (bt:with-lock-held ((guesp-bloqueo frame)) *guesp*)
       do
         (bt:with-lock-held ((guesp-bloqueo frame))
           (when *guesp*
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
    (bt:with-lock-held ((guesp-bloqueo frame)) (setf *guesp* frame))
    (run-frame-top-level frame)
    (bt:with-lock-held ((guesp-bloqueo frame)) (setf *guesp* nil))
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
   (num-cuadro :initform 0 :accessor espacio-num-cuadro)
   (animacion-func :initarg :animacion-func :initform nil :accessor espacio-animacion-func)
   (datos :initform (list) :accessor espacio-datos)))

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
              ((:|R| :|r|) (execute-frame-command *application-frame* `(com-reiniciar)))))))))

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
      (setf (espacio-animacion-func espacio) #'spacewar!
            (espacio-num-cuadro espacio) 1))))

(define-guerra-espacial-command (com-minskytron :name "Minskytron" :menu t)
    ()
  (let ((espacio (find-pane-named *application-frame* 'espacio-pane)))
    (bt:with-lock-held ((guesp-bloqueo *application-frame*))
      (setf (espacio-animacion-func espacio) #'minskytron
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
      (setf (espacio-animacion-func espacio) #'munching-squares
            (espacio-num-cuadro espacio) 1))))

(define-guerra-espacial-command (com-reiniciar :name "Reiniciar" :menu t)
    ()
  (let ((espacio (find-pane-named *application-frame* 'espacio-pane)))
    (bt:with-lock-held ((guesp-bloqueo *application-frame*))
      (setf (espacio-objs espacio) (carga-naves *naves*)
            (espacio-num-cuadro espacio) 1
            (espacio-datos espacio) nil)
      (draw-rectangle* (espacio-pixmap espacio) 0 0
                       (bounding-rectangle-width (sheet-region espacio))
                       (bounding-rectangle-height (sheet-region espacio))
                       :ink +black+))))

(define-guerra-espacial-command (com-salir :name "Salir" :menu t)
    ()
  (bt:with-lock-held ((guesp-bloqueo *application-frame*))
    (frame-exit *application-frame*)))


(defun guerra-espacial-entry-point ()
  (apply 'main *command-line-arguments*))
