(in-package #:guerra-espacial)

(declaim (type double-float *ancho-df* *alto-df*))
(defparameter *ancho* 1024)
(defparameter *alto* 1024)
(defparameter *ancho-df* (coerce *ancho* 'double-float))
(defparameter *alto-df* (coerce *alto* 'double-float))

(declaim (type double-float *max-x* *max-y* *min-x* *min-y*))
(defparameter *max-x* (/ *ancho* 2.0d0))
(defparameter *max-y* (/ *alto* 2.0d0))
(defparameter *min-x* (- (1- *max-x* )))
(defparameter *min-y* (- (1- *max-y* )))

(declaim (type double-float *desp-x* *pausa*))
(defparameter *guesp* nil)
(defparameter *pausa* (/ 40.0d0))
(defparameter *ruta-del-sistema* (asdf:component-pathname (asdf:find-system 'guerra-espacial)))
(defparameter *bloqueo-guesp* (bt:make-lock "Bloqueo para el frame"))
(defparameter *desp-x* (/ (* *pausa* 8192) (* 24 60 60)))
(defparameter *ancho-mapa-estelar* 8192)

(declaim (type double-float *radio-estrella* *2pi* *aceleracion-angular-nave* *aceleracion-nave*
               *gravedad*))
(defparameter *radio-estrella* 5.0d0)
(defparameter *2pi* (* 2.0d0 pi))
(defparameter *aceleracion-angular-nave* (/ (* 8.0d0 pi) 51472.0d0))
(defparameter *aceleracion-nave* 16.0d0)
(defparameter *paso-nave* 2)
(defparameter *gravedad* (/ 12.0d0))

(declaim (type double-float *radio-colision-1* *radio-colision-2*))
(defparameter *radio-colision-1* 96d0)
(defparameter *radio-colision-2* 48d0)
