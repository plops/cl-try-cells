(ql:quickload "cells")
(ql:quickload "cl-actors")
(ql:quickload "optima")
(ql:quickload "bordeaux-threads")


(defpackage :bakery
  (:use :cl :cl-actors :optima :bordeaux-threads :cells)
  (:export #:baker
	   #:cake
   ))

(in-package :bakery)

(defun construct-accumulator ()
  (let ((elems (list)))
    (lambda (el)
      (setf elems (remove nil (adjoin el elems)))
      elems)))

(defvar *all-ingredients-fu* nil)
(defvar *all-dones-fu* nil)

(defmodel cake ()
  ((allingredientsfu :cell nil :accessor allingredientsfu :initform
		   *all-ingredients-fu*)
   (alldonesfu :cell nil :accessor alldonesfu :initform *all-dones-fu*)
   (batter :reader batter :initform '(:milk :egg :flour))
   (icing :reader icing :initform '(:sugar))
   (decoration :reader decoration :initform '(:candles))
   (todos :reader todos :initform '(:knead :bake :decorate))))

