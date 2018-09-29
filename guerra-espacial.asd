;; -*- coding: utf-8-unix; -*-

;;;; Copyright (c) 2018 José Ronquillo Rivera <josrr@ymail.com>
;;;; This file is part of guerra-espacial.
;;;;
;;;; guerra-espacial is free software: you can redistribute it and/or modify
;;;; it under the terms of the GNU General Public License as published by
;;;; the Free Software Foundation, either version 3 of the License, or
;;;; (at your option) any later version.
;;;;
;;;; guerra-espacial is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;; GNU General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU General Public License
;;;; along with guerra-espacial.  If not, see <http://www.gnu.org/licenses/>.

(asdf:defsystem #:guerra-espacial
  :defsystem-depends-on (:deploy)
  :description "Spacewar! clone"
  :author "José Miguel Ronquillo Rivera <jose@rufina.link>"
  :serial t
  :license  "GPL Ver. 3"
  :version "0.0.2"
  :build-operation "deploy-op"
  :build-pathname #P"guerra-espacial"
  :entry-point "guerra-espacial:guerra-espacial-entry-point"
  :depends-on (#:bordeaux-threads #:mcclim)
  :components ((:file "paquetes")
               (:file "parametros")
               (:file "nave")
               (:file "estrella")
               (:file "datos-naves")
               (:file "datos-estrellas")
               (:file "animaciones")
               (:file "presentacion")
               (:file "guerra-espacial")))
