(in-package #:guerra-espacial)

(defparameter *ancho* 1024)
(defparameter *alto* 1024)

(declaim (type double-float *max-x* *max-y* *min-x* *min-y*))
(defparameter *max-x* (/ *ancho* 2.0d0))
(defparameter *max-y* (/ *alto* 2.0d0))
(defparameter *min-x* (- (1- *max-x* )))
(defparameter *min-y* (- (1- *max-y* )))

(declaim (type double-float *desp-x* *pausa*))
(defparameter *guesp* nil)
(defparameter *pausa* 0.015d0)
(defparameter *ruta-del-sistema* (asdf:component-pathname (asdf:find-system 'guerra-espacial)))
(defparameter *bloqueo-guesp* (bt:make-lock "Bloqueo para el frame"))
(defparameter *desp-x* (/ (* *pausa* 8192) (* 24 60 60)))
(defparameter *ancho-mapa-estelar* 8192)

(defparameter *radio-estrella* 10.0d0)
(defparameter *2pi* (* 2.0d0 pi))
(defparameter *aceleracion-angular-nave* (/ (* 8.0d0 pi) 51472.0d0))
(defparameter *aceleracion-nave* 16)
(defparameter *paso-nave* 2)
