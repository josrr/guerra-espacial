;;;; This data is taken from the file published in the following URL
;;;;    https://www.masswerk.at/spacewar/sources/spacewar_2b_2apr62.txt
(in-package #:guerra-espacial)

(defparameter *naves* `((:ot1 (:forma (#o111131
                                       #o111111
                                       #o111111
                                       #o111163
                                       #o311111
                                       #o146111
                                       #o111114
                                       #o700000)
                                      :x 256.0d0
                                      :y 256.0d0
                                      :theta 0.0d0))
                        (:ot2 (:forma (#o013113
                                       #o113111
                                       #o116313
                                       #o131111
                                       #o161151
                                       #o111633
                                       #o365114
                                       #o700000)
                                      :x -256.0d0
                                      :y -256.0d0
                                      :theta ,pi))))
