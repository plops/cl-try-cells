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
   (todos :reader todos :initform '(:knead :bake :decorate))

   (mixin :initform (c-in nil))
   (action :accessor action :initform (c-in nil))

   (ingredients :accessor ingredients :initform (c? (funcall
						     (allingredientsfu self)
						     (mixin self))))
   ;; set-union of the last action and all previous actions
   (dones :accessor dones :initform (c? (funcall (alldonesfu self)
						 (action self))))
   ;; constraint satisfied if subset of ingredients
   (batter-p :accessor batter-p :initform (c? (subsetp (batter self)
						       (ingredients
							self))))
   (alldone-p :accessor alldone-p :initform (c? (not (set-difference
						      (todos self)
						      (dones self)))))))

(defobserver batter-p ((self cake))
  (when new-value
    (format t "Batter is complete.~%")))

(defobserver alldone-p ((self cake))
  (when new-value
    (format t "Cake is done.~%")))

(defobserver ingredients ((self cake))
  (format t "Ingredients are now ~a." new-value))

(defobserver dones ((self cake))
  (format t "Completed tasks are now ~a." new-value))

(defactor baker
    ((*all-ingredients-fu* (construct-accumulator))
     (*all-dones-fu* (construct-accumulator))
     (mycake (make-instance 'cake)))
    (message)
  (match message
	 ((list :add ingredient) when (member ingredient (batter
							   mycake))
	  (when (batter-p mycake)
	    (format t "Batter complete. No need for ~a.~%"
		    ingredient))
	  (if (member ingredient (ingredients mycake))
	      (format t "Already have ~a in batter.~%" ingredient)
	      (setf (mixin mycake) ingredient)))
	 ((list :add ingredient) when (and (member ingredient
						   (decoration
						    mycake))
					   (member :bake (dones
							  mycake)))
	  (if (member ingredient (ingredients mycake))
	      (format t "Already have ~a in cake.~%" ingredient)
	      (setf (mixin mycake) ingredient
		    (action mycake) :decorate)))
	 ((list :act todo) when (member todo (todos mycake))
	  (when (alldone-p mycake)
	    (format t "Cake finished. Decline to do ~a.~%" todo))
	  (if (member todo (dones mycake))
	    (format t "Already did ~a.~%" todo)
	    (ecase todo
	      (:bake (if (member :knead (dones mycake))
			 (setf (action mycake) todo)
			 (format t "Knead batter first. Can't do ~a.~%"
				 todo)))
	      (:knead (if (batter-p mycake)
			  (setf (action mycake) todo)
			  (format t "Batter not ready. Knead dough!
    Not ~a.~%" todo)))
	      (t (format t "Don't know ~a.~%" todo)))))
	 (_ (format t "Recipe error.~%")))
  next)


(defvar *mybaker* (baker))

(send *mybaker* '(:add :flour))

(send *mybaker* '(:act :knead))

(send *mybaker* '(:add :milk))

(send *mybaker* '(:add :eggs))
