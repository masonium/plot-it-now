(defpackage :plot-it-now-system
  (:use :asdf))

(in-package :plot-it-now-system)

(defsystem :plot-it-now
  :description "System for quick plotting of data"
  :author "Mason Smith <masonium@gmail.com>"
  :maintainer "Mason Smith <masonium@gmail.com>"
  :license "WTFPL"
  :depends-on (:alexandria :iterate :lispbuilder-sdl :lispbuilder-sdl-gfx)
  :components
  ((:file "package")
   (:file "iterable-range")
   (:file "plot-it-now"))
  :serial t)
