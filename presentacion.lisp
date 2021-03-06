(in-package #:guerra-espacial)

(defparameter *portada* '#("****************************************************************"
                           "****************************************************************"
                           "****************************************************************"
                           "*****00000**1***1**22222**3333***4444****555********************"
                           "*****0******1***1**2******3***3**4***4**5***5*******************"
                           "*****0******1***1**2******3***3**4***4**5***5*******************"
                           "*****00000**1***1**2222***3333***4444***55555*******************"
                           "*****0***0**1***1**2******3**3***4**4***5***5*******************"
                           "*****0***0**1***1**2******3**3***4**4***5***5*******************"
                           "*****00000***111***22222**3**3***4**4***5***5*******************"
                           "****************************************************************"
                           "****************************************************************"
                           "*****66666***777***8888****999****aaa***bbbbb***ccc***d*********"
                           "*****6******7***7**8***8**9***9**a***a****b****c***c**d*********"
                           "*****6******7******8***8**9***9**a********b****c***c**d*********"
                           "*****6666****777***8888***99999**a********b****ccccc**d*********"
                           "*****6**********7**8******9***9**a********b****c***c**d*********"
                           "*****6******7***7**8******9***9**a***a****b****c***c**d*********"
                           "*****66666***777***8******9***9***aaa***bbbbb**c***c**ddddd*****"
                           "****************************************************************"
                           "****************************************************************"
                           "****************************************************************"
                           "****************************************************************"
                           "****************************************************************"
                           "****************************************************************"
                           "****************************************************************"
                           "****************************************************************"
                           "****************************************************************"
                           "****************************************************************"
                           "*eee************************************************************"
                           "*e*e*ee**eee*ee*****e*e*e*eee*ee**eee***************************"
                           "*e*e***e*e*****e****e*e*e*e*e***e*e*****************************"
                           "*eee*eee*e***eee**e*e*e*e*eee*eee*e*****************************"
                           "*e***e*e*e***e*e**e*e*e*e***e*e*e*e*****************************"
                           "*e***eee*e***eee**eee*eee*eee*eee*e*****************************"
                           "****************************************************************"
                           "****************************************************************"
                           "*eee*eee*eee*eee*e*eee*eee*ee***eee*e*e*ee**e*eee*e*e*e*eee*eee*"
                           "*e*e*e***e*e*e***e*e*e*e*e***e**e***e*e***e*e*e*e*e*e*e*e*e*e***"
                           "*eee*e***eee*eee*e*e*e*e*e*eee**e***e*e*eee*e*eee*e*e*e*eee*e***"
                           "*e***e***e*****e*e*e*e*e*e*e*e**e***e*e*e*e*e***e*e*e*e*e***e***"
                           "*e***e****ee*eee*e*eee*e*e*eee**eee*eee*eee*ee**e*eee*e**ee*e***"
                           "****************************************************************"
                           "****************************************************************"
                           "**e**eee*eee*e**ee**********************************************"
                           "*eee*e*e*e***e****e*********************************************"
                           "**e**eee*e***e**eee*********************************************"
                           "**e**e***e***e**e*e*********************************************"
                           "**e***ee*eee*ee*eee*********************************************"
                           "****************************************************************"
                           "****************************************************************"
                           "****************************************************************"
                           "****************************************************************"
                           "****************************************************************"
                           "****************************************************************"
                           "****************************************************************"
                           "****************************************************************"
                           "*************************f*f*FFF*FFF**********f*****f*****f*****"
                           "*************************f*f*F*F*F***********f*f***f*f***f*f****"
                           "*************************f*f*FFF*F***********f*f***f*f****ff****"
                           "*************************f*f*F***F***********f*f***f*f***f******"
                           "**************************f***FF*F***f********f**f**f**f*fff****"
                           "****************************************************************"
                           "****************************************************************"))

(defun instrucciones (pane)
  (copy-from-pixmap (espacio-pixmap pane) 0 0
                    (bounding-rectangle-width (sheet-region pane))
                    (bounding-rectangle-height (sheet-region pane))
                    pane 0 0))

(defun presentacion (pane)
  (loop repeat *ms-num-cuadros* do
    (with-bounding-rectangle* (x0 y0 x1 y1) (sheet-region pane)
      (climi::with-double-buffering ((pane x0 y0 x1 y1) (wtf))
        (declare (ignore wtf))
        (sleep (/ *pausa* 2))
        (munching-squares pane))))
  (let ((pixmap (espacio-pixmap pane))
        (ancho (bounding-rectangle-width (sheet-region pane)))
        (alto (bounding-rectangle-height (sheet-region pane))))
    (loop repeat 16 do
      (loop with cuantos double-float = *ms-num-cuadros*
            with ancho double-float = (min (/ *ancho-df* cuantos) (/ *alto-df* cuantos))
            with ancho/2 double-float = (/ ancho 2.0d0)
            for i from 0 below *ms-num-cuadros* and x from ancho/2 by ancho
            do (loop for j from 0 below *ms-num-cuadros* and y from ancho/2 by ancho
                     for c = (elt (aref *portada* j) i)
                     do (draw-circle* pane x y ancho/2 :ink (if (or (char=  #\* c)
                                                                    (< (incf (espacio-num-cuadro pane))
                                                                       (* 512 (parse-integer (subseq (aref *portada* j) i (1+ i))
                                                                                             :radix 16))))
                                                                +darkslategrey+
                                                                +cyan+)))))
    (copy-to-pixmap pane 0 0 ancho alto pixmap 0 0)
    (setf (espacio-animacion-func pane) #'instrucciones)))
