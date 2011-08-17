;;;; package.lisp

(cl:in-package :cl-user)

(defpackage :modify-macrolet
  (:use)
  (:export :modify-macrolet))

(defpackage :modify-macrolet-internal
  (:use :modify-macrolet :cl :fiveam))

